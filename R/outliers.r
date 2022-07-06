outliers = function(data){
  qt = as.numeric(summary(data))
  q1 = qt[2]
  q3 = qt[5]
  A = q1 - 1.5*(q3-q1)
  B = q3 + 1.5*(q3-q1)
  ix = data < A | data > B
  outliers = data[ix]
  min.r = min(data[!ix])
  max.r = max(data[!ix])
  dot.5 = c(min.r,q1,qt[3],q3,max.r)
  res = list('dot.5'=dot.5,'outliers'=outliers)
  return(res) 
}
