\name{EEB}
\alias{EEB}
\title{ The Empirical Equivalence Bound
}
\description{ This function calculates the Empirical Equivalence Bound (EEB) at given level.
}
\usage{
EEB(beta, nu, delta = 0, S = 1, alpha = 0.05, 
  type = c("marginal", "cond_NRej", "cond_Rej"), 
  tol = 1e-04, max.itr = 5000)
}
\arguments{
  \item{beta}{ a numeric between 0 and 1. This is the beta value in the EEB definition, see details.
}
  \item{nu}{ an integer, the degrees of freedom in the conventional t-test.
}
  \item{delta}{ a numeric value. Considering testing for difference of two population means, delta is the null value of the difference. Default is 0.
}
  \item{S}{ a numeric value. The standard error in the conventional t-test.
}
  \item{alpha}{ a numeric between 0 and 1. The Type I error rate aiming to control in the conventional t-test.
}
  \item{type}{ a character to specify the type of EEB to be calculated. \code{type = "marginal"} gives the marginal EEB; \code{type = "cond_NRej"} gives the EEB under the condition that one cannot reject the first-stage conventional t-test; \code{type = "cond_Rej"} gives the EEB under the condition that the first-stage conventional t-test is rejected.
}
  \item{tol}{ a numeric value of convergence tolerance.
}
  \item{max.itr}{ an integer, the maximum number of iterations.
}
}
\details{ Consider a two-sample t-test setting with hypotheses
  \deqn{H_{0}:\delta=0 \quad \leftrightarrow \quad H_{1}:\delta\neq 0,}
  where \eqn{\delta=\mu_{1}-\mu_{2}} is the difference of two population means. If the testing result is failure to reject the null, one cannot directly conclude equivalence of the two groups. In this case, an equivalence test is suggested by testing the hypotheses
  \deqn{H_{3}:|\delta|\geq\Delta \quad \leftrightarrow \quad H_{4}:|\delta|<\Delta,}
  where \eqn{\Delta} is a pre-specified equivalence bound. A \eqn{100(1-2\alpha)\%} confidence interval is formulated, denoted as \eqn{[L,U]}, to test for equivalence, where
  \deqn{L=\hat{\delta}-t_{\nu,1-\alpha}S, \quad U=\hat{\delta}+t_{\nu,1-\alpha}S,}
  \eqn{\hat{\delta}} is the estimate of \eqn{\delta}, \eqn{t_{\nu,1-\alpha}} is the \eqn{100(1-\alpha)\%} quantile of a t-distribution with degrees of freedom \eqn{\nu}, and \eqn{S} is the standard error. We define the B-value as
  \deqn{B=\max\{|L|,|U|\},}
  and the Empirical Equivalence Bound (EEB) is defined as
  \deqn{\mathbf{EEB}_{\alpha}(\beta|C)=\inf_{b\in[0,\infty]}\{b:F_{B}(b|C,H_{0})\geq\beta\},}
  where \eqn{\beta\in(0,1)} is a pre-specified level; \eqn{C} denotes the status of the hypothesis test, which takes value of empty set (\code{type = "marginal"}), cannot reject \eqn{H_{0}} (\code{type = "cond_NRej"}), and reject \eqn{H_{0}} (\code{type = "cond_Rej"}); and \eqn{F_{B}(\cdot|C,H_{0})} is the conditional cumulative distribution function of B-value.
}
\value{ Gives the Empirical Equivalence Bound value.
}
\references{Zhao et al. (2019) \emph{"B-Value and Empirical Equivalence Bound: A New Procedure of Hypothesis Testing"} <arXiv:1912.13084>
}
\author{
Yi Zhao, Indiana University, <zhaoyi1026@gmail.com>

Brian Caffo, Johns Hopkins University, <bcaffo@gmail.com>

Joshua Ewen, Kennedy Krieger Institute and Johns Hopkins University, <ewen@kennedykrieger.org>
}
\seealso{\code{\link{pB}}
}
\examples{
#########################################
# R Plant Growth Data

data("PlantGrowth")

PlantGrowth$group
comb.mat<-cbind(c(1,2),c(1,3))
comb.name<-paste0(levels(PlantGrowth$group)[2:3],"-",levels(PlantGrowth$group)[1])
colnames(comb.mat)<-comb.name

alpha<-0.05
# consider a series of beta values
beta.vec<-c(0.5,0.75,0.8,0.9,0.95,0.99)

# two-stage hypothesis testing
# Stage I: conventional two-sample t-test
# Stage II: based on Stage I result to calculate EEB
stat<-matrix(NA,ncol(comb.mat),10+length(beta.vec)*3)
colnames(stat)<-c("delta","LB0","UB0","LB","UB","S","nu","tv","statistic","pvalue",
                  paste0(rep(c("EEB","EEB_NRej","EEB_Rej"),
                    each=length(beta.vec)),"_beta",rep(beta.vec,3)))
rownames(stat)<-comb.name
for(kk in 1:ncol(comb.mat))
{
  x2<-PlantGrowth$weight[which(as.numeric(PlantGrowth$group)==comb.mat[1,kk])]
  x1<-PlantGrowth$weight[which(as.numeric(PlantGrowth$group)==comb.mat[2,kk])]
  
  n1<-length(x1)
  n2<-length(x2)
  
  S<-sqrt((sum((x1-mean(x1))^2)+sum((x2-mean(x2))^2))/(n1+n2-2))*sqrt(1/n1+1/n2)
  
  nu<-n1+n2-2
  
  tv<-qt(1-alpha,df=nu)
  tv2<-qt(1-alpha/2,df=nu)
  
  stat[kk,1]<-mean(x1)-mean(x2)         # delta estimate
  stat[kk,2]<-stat[kk,1]-tv2*S          # (1-alpha)% CI lower bound
  stat[kk,3]<-stat[kk,1]+tv2*S          # (1-alpha)% CI upper bound
  stat[kk,4]<-stat[kk,1]-tv*S           # (1-2alpha)% CI lower bound
  stat[kk,5]<-stat[kk,1]+tv*S           # (1-2alpha)% CI upper bound
  stat[kk,6]<-S                         # standard error
  stat[kk,7]<-nu                        # degrees of freedom in the first-stage t-test
  stat[kk,8]<-tv                        
  stat[kk,9]<-stat[kk,1]/S              # test-statistic in the first-stage t-test
  stat[kk,10]<-(1-pt(abs(stat[kk,9]),df=nu))*2    # p-value in the first-stage t-test
  
  # marginal EEB
  stat[kk,11:(11+length(beta.vec)-1)]<-
    apply(as.matrix(beta.vec),1,
      function(x){return(EEB(beta=x,nu=nu,delta=0,S=S,alpha=alpha,type="marginal"))})
  
  # conditional on not rejection
  if(stat[kk,2]*stat[kk,3]<0)
  {
    stat[kk,(11+length(beta.vec)):(11+length(beta.vec)*2-1)]<-
      apply(as.matrix(beta.vec),1,
        function(x){return(EEB(beta=x,nu=nu,delta=0,S=S,alpha=alpha,type="cond_NRej"))})
  }
  
  # conditional on rejection
  if(stat[kk,2]*stat[kk,3]>0)
  {
    stat[kk,(11+length(beta.vec)*2):(11+length(beta.vec)*3-1)]<-
      apply(as.matrix(beta.vec),1,
        function(x){return(EEB(beta=x,nu=nu,delta=0,S=S,alpha=alpha,type="cond_Rej"))})
  }
}
print(data.frame(t(stat)))

cc<-colors()[c(24,136,564,500,469,50,200,460,17,2,652,90,8,146,464,52,2)]
beta.lgd<-sapply(1:length(beta.vec),
                 function(i){as.expression(substitute(beta==x,
                  list(x=format(beta.vec[i],digit=2,nsmall=2))))})

# Boxplot of data
oldpar<-par(no.readonly=TRUE)
par(mar=c(5,5,1,1))
boxplot(weight~group,data=PlantGrowth,ylab="Dried weight of plants",col=cc[c(3,2,2)],
        pch=19,notch=TRUE,varwidth=TRUE,cex.lab=1.25,cex.axis=1.25)
par(oldpar)

# Comparing t-test CI and equivalence CI using the EEB
# trt1-ctrl
kk<-1
oldpar<-par(no.readonly=TRUE)
par(mar=c(4,5,3,3))
par(oma=c(2.5,0,0,0))
plot(range(c(stat[kk,c(1,2,3,4,5,11:ncol(stat))],-stat[kk,c(11:ncol(stat))]),na.rm=TRUE),
  c(0.5,length(beta.vec)+0.5),type="n",
  xlab="",ylab=expression(beta),yaxt="n",main=comb.name[kk],
  cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
axis(2,at=1:length(beta.vec),labels=FALSE)
text(rep(par("usr")[1]-(par("usr")[2]-par("usr")[1])/100*2,length(beta.vec)),1:length(beta.vec),
  labels=format(beta.vec,nsmall=2,digits=2),srt=0,adj=c(1,0.5),xpd=TRUE,cex=1.25)
for(ll in 1:length(beta.vec))
{
  if(stat[kk,2]*stat[kk,3]<0)
  {
    lines(c(-stat[kk,(10+length(beta.vec))+ll],stat[kk,(10+length(beta.vec))+ll]),rep(ll,2),
      lty=1,lwd=3,col=cc[1])
    points(0,ll,pch=15,cex=1.5,col=cc[1])
    text(-stat[kk,(10+length(beta.vec))+ll],ll,labels="[",cex=1.5,col=cc[1])
    text(stat[kk,(10+length(beta.vec))+ll],ll,labels="]",cex=1.5,col=cc[1])
  }
  if(stat[kk,2]*stat[kk,3]>0)
  {
    lines(c(-stat[kk,(10+length(beta.vec)*2)+ll],stat[kk,(10+length(beta.vec)*2)+ll]),rep(ll,2),
      lty=1,lwd=3,col=cc[1])
    points(0,ll,pch=15,cex=1.5,col=cc[1])
    text(-stat[kk,(10+length(beta.vec)*2)+ll],ll,labels="[",cex=1.5,col=cc[1])
    text(stat[kk,(10+length(beta.vec)*2)+ll],ll,labels="]",cex=1.5,col=cc[1])
  }
  
  lines(stat[kk,c(4,5)],rep(ll,2),lty=1,lwd=3,col=cc[2])
  points(stat[kk,1],ll,pch=19,cex=1.5,col=cc[2])
  text(stat[kk,4],ll,labels="[",cex=1.5,col=cc[2])
  text(stat[kk,5],ll,labels="]",cex=1.5,col=cc[2])
}
par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,2,0,2),new=TRUE)
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom",legend=c("confidence interval","equivalence interval"),xpd=TRUE,horiz=TRUE,
       inset=c(0,0),pch=c(19,15),col=cc[c(2,1)],lty=1,lwd=2,cex=1.5,bty="n")
par(oldpar)

# trt2-ctrl
kk<-2
oldpar<-par(no.readonly=TRUE)
par(mar=c(4,5,3,3))
par(oma=c(2.5,0,0,0))
plot(range(c(stat[kk,c(1,2,3,4,5,11:ncol(stat))],-stat[kk,c(11:ncol(stat))]),na.rm=TRUE),
  c(0.5,length(beta.vec)+0.5),type="n",
  xlab="",ylab=expression(beta),yaxt="n",main=comb.name[kk],
  cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
axis(2,at=1:length(beta.vec),labels=FALSE)
text(rep(par("usr")[1]-(par("usr")[2]-par("usr")[1])/100*2,length(beta.vec)),1:length(beta.vec),
     labels=format(beta.vec,nsmall=2,digits=2),srt=0,adj=c(1,0.5),xpd=TRUE,cex=1.25)
for(ll in 1:length(beta.vec))
{
  if(stat[kk,2]*stat[kk,3]<0)
  {
    lines(c(-stat[kk,(10+length(beta.vec))+ll],stat[kk,(10+length(beta.vec))+ll]),rep(ll,2),
      lty=1,lwd=3,col=cc[1])
    points(0,ll,pch=15,cex=1.5,col=cc[1])
    text(-stat[kk,(10+length(beta.vec))+ll],ll,labels="[",cex=1.5,col=cc[1])
    text(stat[kk,(10+length(beta.vec))+ll],ll,labels="]",cex=1.5,col=cc[1])
  }
  if(stat[kk,2]*stat[kk,3]>0)
  {
    lines(c(-stat[kk,(10+length(beta.vec)*2)+ll],stat[kk,(10+length(beta.vec)*2)+ll]),rep(ll,2),
      lty=1,lwd=3,col=cc[1])
    points(0,ll,pch=15,cex=1.5,col=cc[1])
    text(-stat[kk,(10+length(beta.vec)*2)+ll],ll,labels="[",cex=1.5,col=cc[1])
    text(stat[kk,(10+length(beta.vec)*2)+ll],ll,labels="]",cex=1.5,col=cc[1])
  }
  
  lines(stat[kk,c(4,5)],rep(ll,2),lty=1,lwd=3,col=cc[2])
  points(stat[kk,1],ll,pch=19,cex=1.5,col=cc[2])
  text(stat[kk,4],ll,labels="[",cex=1.5,col=cc[2])
  text(stat[kk,5],ll,labels="]",cex=1.5,col=cc[2])
}
par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,2,0,2),new=TRUE)
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom",legend=c("confidence interval","equivalence interval"),xpd=TRUE,horiz=TRUE,
       inset=c(0,0),pch=c(19,15),col=cc[c(2,1)],lty=1,lwd=2,cex=1.5,bty="n")
par(oldpar)
#########################################
}
\keyword{ models }
