permutation.comb = function(n){
  if(n<3){
    stop("Not enough points!")
  }
  num = factorial(n)/factorial(3)
  da = NULL
  for(i in 1:(n-2)){
    for(j in (i+1):(n-1)){
      for(k in (j+1):n){
        da = as.data.frame(rbind(da,cbind(i,j,k)))
      }
    }
  }
  return(da)
}