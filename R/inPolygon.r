inPolygon = function(d,p,deviation=10){
  p = as.data.frame(p)
  n = length(p$x)
  p$ang <- 'Y'
  d = d[order(d$x),]
  for(i in 1:n){
    ppi = c(p$x[i],p$y[i])
    angle = rep(0,3)
    x = c(1,2,3)
    y = c(2,3,1)
    for(j in 1:3){
      angle[j] = ang(ppi,d[x[j],],d[y[j],])
    }
    if(abs(round(sum(angle),2)-360)>deviation){
      p$ang[i] <- 'N'
    }
  }	     
  return(p)
}