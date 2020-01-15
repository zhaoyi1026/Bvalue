pB <-
function(b,nu,delta=0,S=1,alpha=0.05,type=c("marginal","cond_NRej","cond_Rej"))
{
  if(type[1]=="marginal")
  {
    return(pB.mar(b,nu,delta=delta,S=S,alpha=alpha))
  }
  if(type[1]=="cond_NRej")
  {
    return(pB.conNRej(b,nu,delta=delta,S=S,alpha=alpha))
  }
  if(type[1]=="cond_Rej")
  {
    return(pB.conRej(b,nu,delta=delta,S=S,alpha=alpha))
  }
}
