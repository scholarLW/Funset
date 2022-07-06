mappingsXYZ = function(da,theta=45){
  if(theta>=90 || theta<=0){
    stop("Error theta input!\n")
  }
  if(is.null(da)){
     stop("NULL input!\n")
  }
  n = nrow(da)
  if(n == 0){
    stop("NULL input!\n")
  }
  map = function(x,theta){
    x1 = x[1]+x[3]*cos(theta/180*pi)
    x2 = x[2]+x[3]*sin(theta/180*pi)
    return(cbind(x1,x2))
  }
  d = as.data.frame(t(apply(da,1,map,theta)))
  colnames(d) = c('x','y')
  return(d)
}