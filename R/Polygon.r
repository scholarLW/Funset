sideABC = function(A,B,C){
  SABC = (A[1]-C[1])*(B[2]-C[2])-(A[2]-C[2])*(B[1]-C[1])
  if(SABC>0){
    SABC = 1
  }else 
    if(SABC<0){
    SABC = -1
  }else{
    SABC = 0
  }
  return(SABC)
}

Polygon = function(da,deviation=10){
  d = outPolygon(da,deviation)
  d = as.data.frame(d)
  colnames(d) = c('x','y')
  n = nrow(d)
  d = d[order(d$x),]
  if(n>3){
    index_all = 1:n
    index_po = c(1,2)
    n0 = n - length(index_po)
	index_tmp = setdiff(index_all,index_po)
    while(n0>1){      
      for(i in 1:(n-2)){
        index0 = setdiff(index_all,c(index_po[length(index_po)],index_tmp[i]))
        side_n =rep(0,length(index0))  
        for(j in 1:length(index0)){
          ppi = d[index0[j],]
          side_n[j] = sideABC(d[index_po[length(index_po)],],d[index_tmp[i],],ppi)
        }
        if(sum(side_n)==n-2 || sum(side_n)==2-n){
          index_po = c(index_po,index_tmp[i])
          n0 = n - length(index_po)
		  index_tmp = setdiff(index_all,c(index_po[length(index_po)-1],index_po[length(index_po)]))
        }   
      }
    }
    index_po = c(index_po,setdiff(index_all,index_po))
    d = d[index_po,]
  }
  return(d)
}