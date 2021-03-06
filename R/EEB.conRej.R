EEB.conRej <-
function(beta,nu,delta=0,S=1,alpha=0.05,tol=1e-4,max.itr=5000)
{
  tv<-qt(1-alpha,df=nu)
  
  if(delta==0)
  {
    return(S*(qt(1-(alpha*(1-beta)/2),df=nu)+tv))
  }else
  {
    #----------------------------
    # Bisection method
    a<-S*(tv+qt(1-alpha/2,df=nu))
    b<-100
    
    s<-0
    
    while(s<max.itr)
    {
      c<-(a+b)/2
      
      diff<-(b-a)/2
      
      g<-pB.conRej(c,nu=nu,delta=delta,S=S,alpha=alpha)-beta
      
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
}
