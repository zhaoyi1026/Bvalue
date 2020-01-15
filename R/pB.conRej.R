pB.conRej <-
function(b,nu,delta=0,S=1,alpha=0.05)
{
  tv1<-qt(1-alpha,df=nu)
  tv2<-qt(1-alpha/2,df=nu)
  
  dn<-pt(delta/S-tv2,df=nu)+pt(-tv2-delta/S,df=nu)
  
  if(length(b)==1)
  {
    if(b<S*(tv1+tv2))
    {
      return(0)
    }else
    {
      nu1<-pt((b-delta)/S-tv1,df=nu)-pt(tv2-delta/S,df=nu)
      nu2<-pt(-tv2-delta/S,df=nu)-pt(tv1+(-b-delta)/S,df=nu)
      return((nu1+nu2)/dn)
    }
  }else
  {
    nu1<-pt((b-delta)/S-tv1,df=nu)-pt(tv2-delta/S,df=nu)
    nu2<-pt(-tv2-delta/S,df=nu)-pt(tv1+(-b-delta)/S,df=nu)
    
    out<-(nu1+nu2)/dn
    out[which(b<S*(tv1+tv2))]<-0
    
    return(out)
  }
}
