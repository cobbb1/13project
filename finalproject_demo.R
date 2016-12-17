
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
    pos.get <- pos[j,] - pos[j-1,]
    z <- 0
    for(n in 1:ncol(pos[j,])){
      if (pos.get[n]<0) z <- z - pos.get[n]*open[j,n]*(1+tra)
      if (pos.get[n]>0) z <- z - pos.get[n]*close[j,n]*(1-tra)
    }
    z <- as.numeric(z)
    return(z)
  }
  
  
  getmod = function(amt,open,close,high,low,e){
    if(e==1){
      return(getmod1(amt,open,close,high,low))
    }
  }
  
  getmod1 = function(amt,open,close,high,low){
    mod<-low*0
    for(j in 2:nrow(close)){
      mod[j,] <- close[j-1,]-open[j-1,]
    }
    return(mod)
  }
   
  getsa = function(open,close,amt,high,low,buyable,sellable,mon,mod,tra,e){
    if(e==1){
      return(getsa1(open,close,amt,high,low,buyable,sellable,mon,mod,tra))
    }
  }
  
  getsa1 = function(open,close,amt,high,low,buyable,sellable,mon,mod,tra){
   pos <- low*0
   for(j in 2:nrow(close)){
      #B
      monspe <- 0.1*mon[j-1]
      #buyable?
      modthe <- mod[j,which(buyable[j,]==1)]
      #A
      bought <- which(buyable[j,]==1)[order(modthe)[ncol(modthe)]]
      #calculate the share you buy today
      getsha <- monspe/open[j,bought]/(1+tra)
      #calculate the result of the position matrix of buy
      pos[j,bought] <- getsha
      #calculate the result of the position matrix of sell 
      #which to sell
      sel = which(pos[j-1,]>pos[j,])
      #Can I sell it today
      pos[j,sel[which(sellable[j,sel]==0)]] = pos[j-1,sel[which(sellable[j,sel]==0)]]
      #Calculate the money left today
      mon[j] <- mon[j-1] + getmo(pos,j,tra,open,close)
   }
   write.csv(mon,"mon.csv")
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
  mod <- close*0
  mon <- matrix(0,nrow=nrow(low),ncol=1)
  mon[1] <- 100
  e <- 1
  w <- 1 
  
    
  
  mod <- getmod(amt,open,close,high,low,e)
  position.matrix <- getsa(open,close,amt,high,low,buyable,sellable,mon,mod,transaction,e)
  
  
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
