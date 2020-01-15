pB.conNRej <-
function(b,nu,delta=0,S=1,alpha=0.05)
{
  tv1<-qt(1-alpha,df=nu)
  tv2<-qt(1-alpha/2,df=nu)
  
  dn<-pt(tv2-delta/S,df=nu)-pt(-tv2-delta/S,df=nu)
  
  if(length(b)==1)
  {
    if(b<S*tv1)
    {
      return(0)
    }else
    {
      nu1<-pt(min((b-delta)/S-tv1,tv2-delta/S),df=nu)
      nu2<-pt(max(tv1-(b+delta)/S,-tv2-delta/S),df=nu)
      return((nu1-nu2)/dn)
    }
  }else
  {
    ot1<-apply(as.matrix((b-delta)/S-tv1),1,function(x){return(min(x,tv2-delta/S))})
    nu1<-pt(ot1,df=nu)
    
    ot2<-apply(as.matrix(tv1-(b+delta)/S),1,function(x){return(max(x,-tv2-delta/S))})
    nu2<-pt(ot2,df=nu)
    
    out<-(nu1-nu2)/dn
    out[which(b<S*tv1)]<-0
    
    return(out)
  }
}
