f.fun = function(da){
  ma = matrix(0,5,5)
  constant = matrix(-1,1,5)
  for(i in 1:5){
    ma[,i] = c(da[i,1]^2,da[i,1]*da[i,2],da[i,2]^2,da[i,1],da[i,2])
  } 
  f = c(constant%*%ginv(ma),1)
  return(f)
}

f.f = function(da){
  n = nrow(da)
  if(n!=3){
    stop("Error input!")
  }
  ma = t(matrix(c(sqrt(3)/2,-0.5,1,-sqrt(3)/2,-0.5,1,0,1,1),3,3))
  lamda1 = as.numeric(ginv(ma)%*%da[,1])
  lamda2 = as.numeric(ginv(ma)%*%da[,2])
  D1 = c(-lamda1[1]+lamda1[3],-lamda2[1]+lamda2[3])
  E1 = c(lamda1[1]+lamda1[3],lamda2[1]+lamda2[3])
  da = rbind(da,D1,E1)
  return(da)
}

sarea = function(D,f1,f2,data=NULL,maxIn=.Machine$double.xmax){    
  samf = function(D){
    a = sample(seq(1,10^3,10),3)
    b = sample(seq(1,10^3,10),3)
    c = sample(seq(1,10^3,10),3)
    return(abs(D)/(sum(a)+sum(b)+sum(c)))
  }
  D_Sarea = .Machine$double.xmax/maxIn*samf(D)   
  A = f1[1]-f2[1]*D
  B = f1[2]-f2[2]*D
  C = f1[3]-f2[3]*D
  E = f1[4]-f2[4]*D  
  if(4*A*C-B^2>0){
    #center
    xc = (B*E-2*C*D)/(4*A*C-B^2)
    yc = (B*D-2*A*E)/(4*A*C-B^2)
    aa = 2*(A*xc^2+C*yc^2+B*xc*yc-1)/(A+C+sqrt((A-C)^2+B^2))
    bb = 2*(A*xc^2+C*yc^2+B*xc*yc-1)/(A+C-sqrt((A-C)^2+B^2))
    if(aa>0 && bb>0){
      if(!is.null(data) && nrow(data)>0){
        f = c(A,B,C,D,E,1)
        ellipse = elli.points(f,segments=100)
        n = nrow(data)
        n1 = nrow(ellipse)
        n1_r = 1:(n1-1)
        n1_l = 2:n1
        allinPolygon = 1
        for(i in 1:n){           
          ang_i = NULL
          for(j in 1:(n1-1)){
            ang_i = c(ang_i,ang(data[i,],ellipse[n1_r[j],],ellipse[n1_l[j],]))
          }	
          ang_i = sum(ang_i,na.rm = TRUE)
          if(abs(round(ang_i,2)-360)>10){
            allinPolygon = 0
            break
          }
        }
        if(allinPolygon == 1){
          a = sqrt(aa)
          b = sqrt(bb)
          D_Sarea = pi*a*b
        }
      }else{
        a = sqrt(aa)
        b = sqrt(bb)
        D_Sarea = pi*a*b
      }
    }
  }	
  return(D_Sarea)
}

Minareafill = function(data,da){
  ma = matrix(0,4,4)
  constant = matrix(-1,1,4)
  for(i in 1:4){
    ma[,i] = c(da[i,1]^2,da[i,1]*da[i,2],da[i,2]^2,da[i,2])
  } 
  Q = ginv(ma)
  f1 = as.numeric(constant%*%Q)
  f2 = as.numeric(da[,1]%*%Q)
  
  
  #n = 5
  n = 3
  
  ##################################################################################################
  #an = seq(-n,n,by=0.1)   # change   
  #anl = an[1:(length(an)-1)]
  #an2 = an[2:length(an)]
  #D_min = NULL
  #for(i in 1:(length(an)-1)){
    #D_interval = c(sign(anl[i])*10^abs(anl[i]),sign(an2[i])*10^abs(an2[i])) 	 
    #Dmin = optimize(f = sarea, interval = D_interval, f1, f2, data, 10^n, maximum = FALSE)
    #D_min = rbind(D_min,cbind(Dmin$minimum,Dmin$objective))	 
  #}
  #Dmin = D_min[,1]
  #Dsarea = D_min[,2]
  #ix = Dmin >= -10^n & Dmin <= 10^n 
  #if(sum(ix)==0){
    #stop("Error running in optimize!")
  #}
  #Dmin = Dmin[ix]
  #Dsarea = Dsarea[ix]
  #ix = Dsarea <= 10^n
  #if(sum(ix)==0){
    #stop("Error running in optimize!")
  #} 
  #Dsarea = Dsarea[ix]
  #Dmin = Dmin[ix]  
  ##################################################################################################
  k1 <- k2 <- 0
  step = 0
  while(k1+k2<2 && step<1){
        an = seq(-n,n,by=(1-step))   # change   
        anl = an[1:(length(an)-1)]
        an2 = an[2:length(an)]
        D_min = NULL
        for(i in 1:(length(an)-1)){
          D_interval = c(sign(anl[i])*10^abs(anl[i]),sign(an2[i])*10^abs(an2[i])) 	 
          Dmin = optimize(f = sarea, interval = D_interval, f1, f2, data, 10^n, maximum = FALSE)
          D_min = rbind(D_min,cbind(Dmin$minimum,Dmin$objective))	 
        }
        Dmin = D_min[,1]
        Dsarea = D_min[,2] 
        ix = Dmin >= -10^n & Dmin <= 10^n 
		if(sum(ix)>0){
		   k1 = 1
		   Dmin = Dmin[ix]
		   Dsarea = Dsarea[ix]
		   ix = Dsarea <= 10^n
		   if(sum(ix)>0){
		      k2 = 1
		      Dsarea = Dsarea[ix]
		      Dmin = Dmin[ix]
		   }else{
		     k1 <- k2 <- 0
			 step = step + 0.05
		   }
		}else{
		   k1 <- k2 <- 0
		   step = step + 0.05
		}
  }
  ##################################################################################################
  
  D = Dmin[which.min(Dsarea)]
  A = f1[1]-f2[1]*D
  B = f1[2]-f2[2]*D
  C = f1[3]-f2[3]*D
  E = f1[4]-f2[4]*D  
  f = c(A,B,C,D,E,1)  
  return(f)
}

elli.f <- function(x,y=NULL){
  if(is.null(y)){
    if (is.matrix(x) || is.array(x) || is.data.frame(x) && ncol(x) == 2) {
      y <- x[,2]
      x <- x[,1]
    }else{
      stop("x and y must be vectors, or x must be a 2 column matrix")
    }
  }else
    if(!(is.vector(x) && is.vector(y) && length(x) == length(y))){   
      stop("x and y must be vectors of the same length")  
    }
  
  require(MASS)  
  f = rep(0,6)
  n = length(x)
  
  # slope
  if(n == 1){   
    center = c(sum(x)/length(x),sum(y)/length(y))
    f=c(1,0,1,-2*center[1],-2*center[2],center[1]^2+center[2]^2-0.01^2)      
  }else{
    da = as.data.frame(cbind(x,y))
    da = collinear(da)
    da = outPolygon(da)
    n = nrow(da)   
    # remove almost similar points
    if(n>2){
      da.tmp = NULL
      min.lenS = 0.005
      for(i in 1:(n-1)){
        for(j in (i+1):n){
          dc_ij = dc(da[i,],da[j,])
          da.tmp = rbind(da.tmp,cbind(i,j,dc_ij))
        }     
      }
      ix = da.tmp[,3]/max(da.tmp[,3]) < min.lenS
      if(sum(ix)>0){
        da = da[-da.tmp[ix,1],]
        n = nrow(da)
      }   
    }   
    if(n == 2){
      addps = add.points(da,perc=0.1,ang=60)
      x = addps[1,]
      da = rbind(da,x)
      da_f = f.f(da)   
      f = f.fun(da_f)
    }    
    if(n == 3){ 	     
      da_f = f.f(da)  
      f = f.fun(da_f)
    }
    if(n >= 4){
      # peripheral four points	
      dc_gr = da		  
      ix = which.min(dc_gr$x)
      x.l = dc_gr[ix,]
      da = da[-ix,]
      dc_gr = dc_gr[-ix,]
      ix = which.max(dc_gr$x)
      x.r = dc_gr[ix,]
      da = da[-ix,]
      dc_gr = dc_gr[-ix,]	
      ix = which.max(dc_gr$y)
      y.t = dc_gr[ix,]
      dc_gr = dc_gr[-ix,]		
      da = da[-ix,]
      ix = which.min(dc_gr$y)
      y.b = dc_gr[ix,]
      dc_gr = dc_gr[-ix,]  
      da = da[-ix,]
      dc_gr = rbind(x.l,x.r,y.t,y.b)
      colnames(dc_gr) = c('x','y')
      f = Minareafill(da,dc_gr)
    }
  } 
  return(f)
}