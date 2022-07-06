collinear = function(da,deviation=0.01){
  slope = function(x,y){ 
    x = as.numeric(x)
    y = as.numeric(y)	
    if(x[1] == y[1]){
      k =  Inf
    }else{
      k =  (y[2]-x[2])/(y[1]-x[1])
    }  
    return(k)
  }   
  n = nrow(da)
  if(n>2){
    rm.index = NULL
    index = permutation.comb(n)
    for(i in 1:nrow(index)){
      a = da[index$i[i],]
      b = da[index$j[i],]
      c = da[index$k[i],]
      kab = slope(a,b)
      kac = slope(a,c)
      if(abs(kab-kac)<deviation){
        dcab = dc(a,b)
        dcac = dc(a,c)
        dcbc = dc(b,c)
        if(dcab > dcac){
          if(dcab < dcbc){
            rm.index = c(rm.index,index$i[i])			  
          }else{
            rm.index = c(rm.index,index$k[i])
          }
        }else{
          if(dcac < dcbc){
            rm.index = c(rm.index,index$i[i])			  
          }else{
            rm.index = c(rm.index,index$j[i])
          }			  
        }
      }
    }
    rm.index = unique(sort(rm.index))
    if(length(rm.index)>0){
      da = da[-rm.index,]
    }
  }
  return(da)
}
