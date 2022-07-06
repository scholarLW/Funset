elli.points <- function(f=NULL,segments=1000){
  if(is.null(f) || !is.vector(f) || (is.vector(f) && length(f)!=6)){
     stop("f must be vectors, and it's length must be 6!")
  }
  # f <- c(A,B,C,D,E,F)
  delta = (2*f[2]*f[5]-4*f[3]*f[4])^2-4*(f[2]^2-4*f[1]*f[3])*(f[5]^2-4*f[3]*f[6]) #########
  if(delta < 0){
    delta = abs(delta)	############
  }
  xmin = min(c((-(2*f[2]*f[5]-4*f[3]*f[4])-sqrt(delta))/(2*(f[2]^2-4*f[1]*f[3])),(-(2*f[2]*f[5]-4*f[3]*f[4])+sqrt(delta))/(2*(f[2]^2-4*f[1]*f[3]))))
  xmax = max(c((-(2*f[2]*f[5]-4*f[3]*f[4])-sqrt(delta))/(2*(f[2]^2-4*f[1]*f[3])),(-(2*f[2]*f[5]-4*f[3]*f[4])+sqrt(delta))/(2*(f[2]^2-4*f[1]*f[3]))))
  x = seq(xmin,xmax+0.1,by=(xmax-xmin)/segments)
  n = length(x)
  y_top <- rep(NA,n)
  y_bottom <- rep(NA,n)
  for(i in 1:n){
    x0 = x[i]
    delta_x0 = (x0*f[2]+f[5])^2-4*f[3]*(f[1]*x0^2+f[4]*x0+f[6])
    if(delta_x0 >= 0){
      y_top[i] = max(c((-(x0*f[2]+f[5])-sqrt(delta_x0))/(2*f[3]),(-(x0*f[2]+f[5])+sqrt(delta_x0))/(2*f[3])))
      y_bottom[1+n-i] = min(c((-(x0*f[2]+f[5])-sqrt(delta_x0))/(2*f[3]),(-(x0*f[2]+f[5])+sqrt(delta_x0))/(2*f[3])))
    } 
  }
  ellipse = as.data.frame(rbind(cbind(x,y_top),cbind(rev(x),y_bottom)))
  colnames(ellipse) = c('x','y')
  ix = is.na(ellipse$y) | is.null(ellipse$y)
  ellipse = ellipse[!ix,]
  ellipse = rbind(ellipse,ellipse[1,])
  return(ellipse)
}