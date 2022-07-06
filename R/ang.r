ang = function(A,B,C){
  # |AC| and |AB|
  AC = sqrt((A[2]-C[2])^2+(A[1]-C[1])^2)
  AB = sqrt((A[2]-B[2])^2+(A[1]-B[1])^2)  	  
  BAC = (A[2]-C[2])*(A[2]-B[2])+(A[1]-C[1])*(A[1]-B[1])
  cos_ang = BAC/(AB*AC)
  angN = as.numeric(acos(cos_ang)/pi*180)
  return(angN)
}