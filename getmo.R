getmod = function(amt,open,close,high,low,j,e){
  if(e==1){
    return(getmod1(amt,open,close,high,low,j))
  }
}

getmod1 = function(amt,open,close,high,low,j){
  return(close[j-1,]-open[j-1,])
}