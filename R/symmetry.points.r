symmetry.points = function(da,k,l){
  #x,y
  da = as.data.frame(da)
  colnames(da) = c('x','y')
  n = nrow(da)  
  tmp = NULL
  for(i in 1:n){
    x0 = da$x[i]
    y0 = da$y[i]
    x1 = ((k-1/k)*x0+2*l-2*y0)/(-k-1/k)
    y1 = y0-1/k*(x1-x0)		
    tmp = as.data.frame(rbind(tmp,cbind(x1,y1)))
  }
  #x,y
  colnames(tmp) = c('x','y')
  tmp = tmp[order(tmp$y),]
  return(as.data.frame(unique(tmp)))
}	