basic.5points = function(data,perc=0.2,ang=60){      
  data = data[order(data$x),]
  n = nrow(data)
  tmp = NULL
  for(i in 2:n){
    tmp = as.data.frame(rbind(tmp,cbind(1,i,dc(data[1,],data[i,]))))
  }  
  colnames(tmp) = c('index1','index2','dc')
  ix = which.max(tmp$dc)
  index = as.numeric(tmp[ix,][,1:2])
  
  step = 0 
  while(n < 5){  
    data = unique(rbind(data,add.points(data[index,],perc,ang)))
    data = collinear(data)
    data = outPolygon(data)
    n = nrow(data)
    if(n>5){
      data = data[1:5,]
    }
    data = data[order(data$x),]
    if(step == 2){
      perc = perc + 0.1
      cat('perc',perc,"\n")
      step = 0
    }
    step = step+1 
  }
  return(data)
}

dist3p.max = function(da,center,perc=0.2,ang=60){
  if(nrow(da)!=3){
    stop("The length must be 3!")
  }
  index = 1:3
  id = as.numeric(dc.max(da))
  index_other = setdiff(index,id)
  add.cp = symmetry.center(da[index_other,],center)
  add.lp = symmetry.center(da[id[1],],center)
  da = as.data.frame(rbind(da,add.cp,add.lp))
  da = collinear(da)
  da = outPolygon(da)
  n = nrow(da)
  if(n < 5){
    add.lp = symmetry.center(da[id[2],],center)
  }
  da = unique(as.data.frame(rbind(da,add.cp,add.lp)))
  da = collinear(da)
  da = outPolygon(da)
  n = nrow(da)
  if(n < 5){
    da = basic.5points(da,perc,ang)
  } 
  return(da)
}

f.fun = function(da){
  ma = matrix(0,5,5)
  constant = matrix(-1,1,5)
  for(i in 1:5){
    ma[,i] = c(da[i,1]^2,da[i,1]*da[i,2],da[i,2]^2,da[i,1],da[i,2])
  } 
  f = c(constant%*%ginv(ma),1)
  return(f)
}

elli.f.old <- function(x,y=NULL,perc=0.2,ang=60){
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
    
    if(n == 5){
      da_f = da 
    }else
      if(n == 2){
        addps = add.points(da,perc,ang)
        x = addps[1,]
        da = rbind(da,addps)
        if(da[2,1]==da[1,1]){
          symps = c(2*da[1,1]-x[1],x[2])
        }else
          if(da[2,2]==da[1,2]){
            symps = c(x[1],2*da[1,2]-x[2])		 
          }else{
            k = (da[2,2]-da[1,2])/(da[2,1]-da[1,1])
            l = da[2,2] - k*da[2,1]
            symps = symmetry.points(x,k,l)
          }
        da_f = rbind(da,symps)		 
      }else
        if(n == 3){ 	     
          center = c(sum(da$x)/n,sum(da$y)/n)
          da_f = dist3p.max(da,center,perc,ang)
        }else		
          if(n == 4){
            da_f = basic.5points(da,perc,ang)
          }else{
            # peripheral four points	
            dc_gr = da		  
            ix = which.min(dc_gr$x)
            x.l = dc_gr[ix,]
            dc_gr = dc_gr[-ix,]
            ix = which.max(dc_gr$x)
            x.r = dc_gr[ix,]
            dc_gr = dc_gr[-ix,]	
            ix = which.max(dc_gr$y)
            y.t = dc_gr[ix,]
            dc_gr = dc_gr[-ix,]		
            ix = which.min(dc_gr$y)
            y.b = dc_gr[ix,]
            dc_gr = dc_gr[-ix,]  
            a = c(x.l[1],y.b[2])
            b = c(x.l[1],y.t[2])
            c = c(x.r[1],y.t[2])
            d = c(x.r[1],y.b[2])		  
            da = as.data.frame(rbind(a,b,c,d)) 
            colnames(da) = c('x','y')
            da_f = basic.5points(da,perc,ang)
          }
    
    f = f.fun(da_f)
  }	
  return(f)
}