pB.mar <-
function(b,nu,delta=0,S=1,alpha=0.05)
{
  tv<-qt(1-alpha,df=nu)
  
  out<-rep(NA,length(b))
  out[which(b<S*tv)]<-0
  itmp<-which(b>=S*tv)
  
  if(length(itmp)>0)
  {
    p1<-pt(-tv+(b[itmp]-delta)/S,df=nu)
    p2<-pt(tv+(-b[itmp]-delta)/S,df=nu)
    
    out[itmp]<-p1-p2
  }
  
  return(out)
}
