dc.max = function(da){
  n = nrow(da)
  tmp = NULL
  for(i in 1:(n-1)){
    tmp = rbind(tmp,cbind(i,i+1,dc(da[i,],da[i+1,])))
  }
  colnames(tmp) = c('index1','index2','dc')
  tmp = as.data.frame(rbind(tmp,cbind('index1'=n,'index2'=1,'dc'=dc(da[n,],da[1,]))))
  ix = which.max(tmp$dc) 
  if(sum(ix)>1){
    index = tmp[ix,][,1:2][1,]
  }else{
    index = tmp[ix,][,1:2]
  }   
  return(index)
}