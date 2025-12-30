\name{WPL}
\alias{WPL}
\title{
Nonparametric maximum likelihood estimate of CDF from left truncated, right censored data.
}
\usage{
WPL(x, d, w=rep(1, length(d)), y=rep(-Inf, length(d)))
}
\arguments{
\item{x}{
a vector of numbers. 
The possibly right censored, observed lifetimes.
}
\item{d}{
a vector of 0 or 1. same length as x.
The censoring status. d=1, not censored. d=0, right censored.
}
\item{w}{
a vector of positive numbers, same length as x.
If w=2 then there are two observations with same x value and d value.
}
\item{y}{
optional vector holding the truncation times. If there is no truncation, then y= -Inf.
}
}
\value{
a list contain the following vectors: 

times: the time points where there is a positive probability mass

survjump: probability mass at the corresponding time points.

surv:  survival probabilities at the time points. If there is ambiguity, see the jumps.

CumHaz: the cumulative hazard, at the times.
}
\description{
This function will compute the nonparametric maximum likelihood 
estimator of survival/distribution function from left truncated, right censored data. It is some times called the product-limit estimator, the Lynden-Bell (1971) estimator or 
the Tsai-Jewell-Wang estimator.  

This function is included here mainly for easy comparison of the NPMLE to the NP Bayes. 
}
\details{
Due to left truncation,
data must also satisfy \eqn{y_i < x_i}. This function is standalone.
}
\references{
Lynden-Bell, D. (1971).
A method of allowing for known observational selection in small samples applied to 3CR quasars 
Monthly Notices Roy. Astronom. Soc. 155, 95-118.

Luan, J, and Zhou, M. (2004). 
Nonparametric Bayes estimator of survival functions for
censored data that are also subject to left truncation.
Tech Report. Univ. Kentucky.
}
\author{ Mai Zhou. }
\examples{
trunc <- c(2,4,10)
xtime <- c(9,13,15)
d <- c(1,0,0) 
NPBayesT(B=8, theta=0.12, u=3.2, y=trunc, x=xtime, status=d)

ypsy <- c(51, 58, 55, 28, 25, 48, 47, 25, 31, 30, 33, 43, 45, 35, 36)
xpsy <- c(52, 59, 57, 50, 57, 59, 61, 61, 62, 67, 68, 69, 69, 65, 76)
dpsy <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1 ) 
WPL(x=xpsy, d=dpsy, w=rep(1, length(d)),y=ypsy)
}
\keyword{nonparametric}
\keyword{survival}