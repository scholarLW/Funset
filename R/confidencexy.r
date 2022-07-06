confint<-function(x,sigma=-1,alpha=0.05)
{
  n<-length(x)
  xb<-mean(x)
  if(sigma>=0)
  {
    tmp<-sigma/sqrt(n)*qnorm(1-alpha/2);df<-n
  }
  else{
    tmp<-sd(x)/sqrt(n)*qt(1-alpha/2,n-1);df<- n-1
  }
  a = data.frame(mean=xb,df=df,a=xb-tmp,b=xb+tmp)
  return(a)
}

confidencexy = function(x,level=0.95,sigma=-1){
  x.level = confint(x,sigma,1-level)  
  return(x.level)
}