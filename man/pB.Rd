\name{pB}
\alias{pB}
\title{ The B-Value Distribution
}
\description{ This function gives the cumulative distribution function of the B-value.
}
\usage{
pB(b, nu, delta = 0, S = 1, alpha = 0.05, type = c("marginal", "cond_NRej", "cond_Rej"))
}
\arguments{
  \item{b}{ vector of quantiles
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
}
\details{ Consider a two-sample t-test setting with hypotheses
  \deqn{H_{0}:\delta=0 \quad \leftrightarrow \quad H_{1}:\delta\neq 0,}
  where \eqn{\delta=\mu_{1}-\mu_{2}} is the difference of two population means. If the testing result is failure to reject the null, one cannot directly conclude equivalence of the two groups. In this case, an equivalence test is suggested by testing the hypotheses
  \deqn{H_{3}:|\delta|\geq\Delta \quad \leftrightarrow \quad H_{4}:|\delta|<\Delta,}
  where \eqn{\Delta} is a pre-specified equivalence bound. A \eqn{100(1-2\alpha)\%} confidence interval is formulated, denoted as \eqn{[L,U]}, to test for equivalence, where
  \deqn{L=\hat{\delta}-t_{\nu,1-\alpha}S, \quad U=\hat{\delta}+t_{\nu,1-\alpha}S,}
  \eqn{\hat{\delta}} is the estimate of \eqn{\delta}, \eqn{t_{\nu,1-\alpha}} is the \eqn{100(1-\alpha)\%} quantile of a t-distribution with degrees of freedom \eqn{\nu}, and \eqn{S} is the standard error. We define the B-value as
  \deqn{B=\max\{|L|,|U|\}.}
  The cumulative distribution function of the B-value is defined under three conditions: (1) the marginal distribution (\code{type = "marginal"}); (2) the conditional distribution given that one cannot reject \eqn{H_{0}} in the conventional t-test (\code{type = "cond_NRej"}); and (3) the conditional distribution given that \eqn{H_{0}} is rejected in the conventional t-test (\code{type = "cond_Rej"}).
}
\value{ Gives the cumulative distribution function of the B-value.
}
\references{Zhao et al. (2019) \emph{"B-Value and Empirical Equivalence Bound: A New Procedure of Hypothesis Testing"} <arXiv:1912.13084>
}
\author{
Yi Zhao, Indiana University, <zhaoyi1026@gmail.com>

Brian Caffo, Johns Hopkins University, <bcaffo@gmail.com>

Joshua Ewen, Kennedy Krieger Institute and Johns Hopkins University, <ewen@kennedykrieger.org>
}
\seealso{\code{\link{EEB}}
}
\examples{
############################################
# An Example: demonstration of marginal/conditional distribution of the B-value
alpha<-0.05

delta<-0
n1=n2=n<-10
S<-0.325
nu<-n1+n2-2

# compare three types of B-value distributions
oldpar<-par(no.readonly=TRUE)
par(mar=c(6,5,2,2))
plot(c(0,2),c(0,1),type="n",xlab=expression(b),ylab=expression(F[B](b*~"|"*~C,H[0])),
     cex.lab=1.25,cex.axis=1.25,cex.main=1.25)
abline(h=1,lty=1,lwd=2,col=8)
abline(h=0,lty=1,lwd=2,col=8)
curve(pB(x,nu=nu,delta=delta,S=S,alpha=alpha,type="marginal"),
      lty=1,lwd=3,col=1,n=1000,from=0,to=20,add=TRUE)
curve(pB(x,nu=nu,delta=delta,S=S,alpha=alpha,type="cond_NRej"),
      lty=2,lwd=3,col=2,n=1000,from=0,to=20,add=TRUE)
curve(pB(x,nu=nu,delta=delta,S=S,alpha=alpha,type="cond_Rej"),
      lty=3,lwd=3,col=4,n=1000,from=0,to=20,add=TRUE)
par(fig=c(0,1,0,1),oma=c(0,0,0,0),mar=c(0,2,0,2),new=TRUE)
plot(0,0,type="n",bty="n",xaxt="n",yaxt="n")
legend("bottom",legend=c("marginal","conditional (not reject)","conditional (reject)"),
       xpd=TRUE,horiz=TRUE,inset=c(0,0),col=c(1,2,4),lty=c(1,2,3),lwd=2,bty="n",cex=1.25)
par(oldpar)
############################################
}
\keyword{ models }
