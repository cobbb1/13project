getsa = function(open,close,amt,high,low,buyable,mon,mod,j,e){
  if(e==1){
    return(getsa1(open,close,amt,high,low,buyable,mon,mod,j))
  }
}

getsa1 = function(open,close,amt,high,low,buyable,mon,mod,j){
  pos <- matrix(1,ncol(close))
  tra <- 0.008
  mon <- 0.1*mon
  bought <- order(mod)[1]
  pos <- close[1,]*0
  getsha <- mon/open[j,bought]/(1+tra)
  pos[bought] <- getsha
  return(pos)
  
}