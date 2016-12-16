
load("MSBD5013_Final_Project.RData")





strategy <- function(open, high, low, close, amt, buyable, sellable, initial.cash, transaction){
  
  ## necessary packages
  require(TTR)  # functions about technical indicators such as moving average
  
  
  ## your own convenience functions 
  ## (please give comments about their purpose and the meaning of each argument)
  pushback <- function(z, L=1){
    # create a new matrix with all values moved downward L rows
    # z is a matrix, L is a integer
    z <- as.matrix(z)  
    A <- matrix(NA, nrow=L, ncol=ncol(z))
    y <- z
    y[1:L,] <- A 
    y[-(1:L),] <- as.matrix(z[-((nrow(z)-L+1):nrow(z)), ])
    return(y)
  }
  
  pushforward <- function(z, L=1){
    # create a new matrix with all values moved upward L rows
    # z is a matrix, L is a integer
    z <- as.matrix(z) 
    A <- matrix(NA, nrow=L, ncol=ncol(z))
    y <- z
    y[1:(nrow(z)-L), ] <- z[-(1:L),]
    y[(nrow(z)-L+1):nrow(z),]<- A
    return(y)
  }
  
  
  getmo = function(pos,j,tra,open,close){
    pos.get = pos[j,]-pos[j-1,]
    z = 0
    for(n in 1:ncol(pos[j,])){
      if (pos.get[n]<0){
        z <- z - pos.get[n]*open[j,n]*(1+tra)
      }
      if (pos.get[n]>0){
        z <- z - pos.get[n]*close[j,n]*(1-tra)
      }
      
    }
    return(z)
  }
  
  
  getmod = function(amt,open,close,high,low,j,e){
    if(e==1){
      return(getmod1(amt,open,close,high,low,j))
    }
  }
  
  getmod1 = function(amt,open,close,high,low,j){
    return(close[j-1,]-open[j-1,])
  }
  
  getsa = function(open,close,amt,high,low,buyable,mon,mod,j,e){
    if(e==1){
      return(getsa1(open,close,amt,high,low,buyable,mon,mod,j))
    }
  }
  
  getsa1 = function(open,close,amt,high,low,buyable,mon,mod,j){
    pos <- matrix(1,ncol(close))
    tra <- 0.008
    mon <- 0.1*mon
    modthe = mod[j,] * buyable[j,]
    bought <- order(modthe)[ncol(close)]
    pos <- close[1,]*0
    getsha <- mon/open[j,bought]/(1+tra)
    pos[bought] <- getsha
    return(pos)
    
  }
  
  ## the main part of your strategy
  ## (please describe the idea of your strategy in the beginning, and give comments on key codes)
  
  # strategy: 
  # buy condition: for each stock, when observing 5-day moving average line upcrossing 60-day moving average line 
  # (moving averages are calculated by close price), buy this stock tomorrow at openning price (need satisfy buyable
  # condition). The investment money should be around 5. When more than one stocks satisfy buy condition, just pick
  # one stock with highest amount yesterday
  # sell condition: for each stock in current position, when observing 5-day moving average line downcrossing 60-day 
  # moving average line, sell this stock tomorrow at closing price (need satisfy sellable condition)
  # other settings: when observing available cash is less than 10, stop any buying actions
  
  reserved.cash <- 10
  investment.cash <- 5
  
  MA5 <- apply(close, 2, SMA, n=5)  # 5-day simple moving average
  MA60 <- apply(close, 2, SMA, n=60)  # 60-day simples moving average
  
  lower <- (MA5 < MA60)  # indicates when MA5 is lower than MA60
  upper <- (MA5 >= MA60)  # indicates when MA5 is higher or equal than MA60
  
  upcrossing <- pushback(lower)*upper  
  # an upcross happens when yesterday's MA5 is lower than MA60 but today's MA5 is higher or equal than MA60
  downcrossing <- pushback(upper)*lower
  # a downcross happens when yesterday's MA5 is higher or equal than MA60 but today's MA5 is lower than MA60
  
  buy.signal <- pushback(upcrossing)*buyable
  # a buy signal happens when observed upcross yesterday, and the stock is "buyable" today
  sell.signal <- pushback(downcrossing)*sellable
  # a sell signal happens when observed downcross yesterday, and the stock is "sellable" today
  
  available.cash <- initial.cash  # initialize available cash
  position.matrix <- close*0  # initialize position matrix
  mon <- 100
  mod <- close*0
  e <- 1
  w <- 1 
  for(j in 2:nrow(close)){
    mod[j,] <- getmod(amt,open,close,high,low,j,e)
    position.matrix[j,] <- getsa(open,close,amt,high,low,buyable,mon,mod,j,e)
    mon <- mon + getmo(position.matrix,j,transaction,open,close)
  }
  
  ## return position.matrix
  return(position.matrix)
}

start <- 1
end <- nrow(CLOSE)

open <- OPEN[start:end,]
close <- CLOSE[start:end,]
high <- HIGH[start:end,]
low <- LOW[start:end,]
amt <- AMT[start:end,]
buyable <- BUYABLE[start:end,]
sellable <- SELLABLE[start:end,]

position.matrix <- strategy(open, high, low, close, amt, buyable, sellable, initial.cash, transaction)
write.csv(position.matrix,"qwe.csv")
check(open, high, low, close, amt, buyable, sellable, initial.cash, transaction, position.matrix)
performance.summary(open, high, low, close, amt, buyable, sellable, initial.cash, transaction, position.matrix)
