add.points = function(da,perc=0.1,ang=30){
  l = sqrt((da[2,1]-da[1,1])^2+(da[2,2]-da[1,2])^2)/2
  c = c(sum(da[,1])/2,sum(da[,2])/2) 
  ll = perc*l
  if(da[1,1] == da[2,1]){
    k = 1
  }else
    if(da[1,2] == da[2,2]){
      k = -1   		 
    }else{
      k = (da[2,2]-da[1,2])/(da[2,1]-da[1,1])
      if(k < 0){
        ang = -ang
      }
      angle = atan(k)/pi*180 + ang
      if(sin(angle/180*pi) == 1){
        angle = 45
      }
      if(sin(angle/180*pi) == -1){
        angle = -45
      }	  
      k = tan(angle/180*pi)
    }
  a = c(c[1]+ll/sqrt(1+k^2),c[2]+ll*k/sqrt(1+k^2))
  b = c(c[1]-ll/sqrt(1+k^2),c[2]-ll*k/sqrt(1+k^2))	
  da = as.data.frame(rbind(a,b))
  colnames(da) = c('x','y')	
  return(da)
}	