EEB <-
function(beta,nu,delta=0,S=1,alpha=0.05,type=c("marginal","cond_NRej","cond_Rej"),tol=1e-4,max.itr=5000)
{
  if(type[1]=="marginal")
  {
    return(EEB.mar(beta,nu,delta=delta,S=S,alpha=alpha,tol=tol,max.itr=max.itr))
  }
  if(type[1]=="cond_NRej")
  {
    return(EEB.conNRej(beta,nu,delta=delta,S=S,alpha=alpha,tol=tol,max.itr=max.itr))
  }
  if(type[1]=="cond_Rej")
  {
    return(EEB.conRej(beta,nu,delta=delta,S=S,alpha=alpha,tol=tol,max.itr=max.itr))
  }
}
