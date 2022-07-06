dc.centerMin = function(x,y,dis=10^-3,error=10^-3){ 
  L = dc(x,y)
  if(L <= dis){
    dis = L/100
    error = 10^-5
  }
  while(abs(L-dis) > error){     
      x = c(sum(x[1]+y[1])/2,sum(x[2]+y[2])/2)
      L = dc(x,y)
  } 
  return(x) 
}