EEB.mar <-
function(beta,nu,delta=0,S=1,alpha=0.05,tol=1e-4,max.itr=5000)
{
  tv<-qt(1-alpha,df=nu)
  
  #----------------------------
  # Bisection method
  a<-0
  b<-100
  
  s<-0
  
  while(s<max.itr)
  {
    c<-(a+b)/2
    
    diff<-(b-a)/2
    
    g<-pB(c,nu=nu,delta=delta,S=S,alpha=alpha)-beta
    
    if(abs(g)<tol|diff<tol)
    {
      break
    }else
    {
      if(g<0)
      {
        a<-c
      }else
      {
        b<-c
      }
      
      s<-s+1
    }
  }
  
  return(c)
  #----------------------------
}
