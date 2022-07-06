outPolygon = function(da,deviation=10){
  n = nrow(da)
  index_all = 1:n
  if(n>3){
    rm.index = NULL
    index = permutation.comb(n)
    for(i in 1:nrow(index)){
      index0 = as.numeric(index[i,])
      index_just = setdiff(index_all,index0)
      d = da[index0,]
      p = da[index_just,]
      p = inPolygon(d,p,deviation)
      ix = p$ang == 'Y'
      if(sum(ix)>0){
        rm.index = c(rm.index,index_just[ix])
      }		
    }
    rm.index = unique(sort(rm.index))
    if(length(rm.index)>0){
      da = da[-rm.index,]
    }
  }
  return(da)
}
