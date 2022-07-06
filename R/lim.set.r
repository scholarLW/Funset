 lim.cal = function(x){
   k = 0
   if(x != 0){
    if(log10(abs(x))>1){
      k = sign(x)*10^trunc(log10(abs(x)))
	  k = k*round(abs(x/k)+0.05,1)
	}else{	  
      k = sign(x)*10^trunc(abs(log10(abs(x))))
	  k = sign(k)*1/abs(k)*round(abs(x*k)+0.05,1)	  
	}
   }	
    return(k)
 } 
lim.set = function(x){
	xr = max(x)
	xl = min(x)
	if(xr > 0 && xl < 0){
		maxd = min(c(abs(xl),xr))/2	
	}else{
		maxd = (xr - xl)/5
	}
    if(length(x)==2){
	   kk = round(max(c(0.0051,maxd)),2)
	}else{
	  	kk = lim.cal(maxd) 
	}	
	xr1 = trunc(xr/kk)
	xl1 = trunc(xl/kk)
	while(xr1*kk < xr){
	  xr1 = xr1 + 1
	}
	while(xl1*kk > xl){
	  xl1 = xl1 + 1*sign(xl1)
	}
    xr = xr1*kk
    xl = xl1*kk
	return(list('r'=xr,'l'=xl,'scale'=kk))
} 
 
 