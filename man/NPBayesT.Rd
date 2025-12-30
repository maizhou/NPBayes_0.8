\name{NPBayesT}
\alias{NPBayesT}
\title{
Nonparametric Bayes estimate of CDF from left truncated, right censored data
}
\usage{
NPBayesT(B, theta, u, y = numeric(0), x = numeric(0), status = numeric(0))
}
\arguments{
\item{B}{
a positive number. The parameter for the Dirichlet process prior. 
It is the weight of prior information. 
If B is very small, then the resulting
prior is "non-informative".
}
\item{theta}{
a positive number. Another parameter for Dirichlet process prior. 
The measure/parameter is \eqn{ \alpha [t, \infty ) = B \exp( - \theta t)} .
}
\item{u}{
a non-negative number, where the Bayes estimator of
\eqn{ 1- \hat F(u)} is to be computed.
}
\item{y}{
optional vector holding the (non-negative) left truncation times. If some x has left truncation, some do not; then those do not have left truncation we simply set y=0 (smallest possible value).
}
\item{x}{
optional vector holding the (possibly) right censored observations.
( x_i must > y_i ).
}
\item{status}{
optional vector holding the censoring status of the x vector.
1--uncensor, 0--censor.
}
}
\value{
a single value that is 
the nonparametric Bayes estimator \eqn{ 1- \hat F(u) }.  
}
\description{
This function will compute the nonparametric Bayes estimator of
survival/distribution function from left truncated, right
censored data with square error loss. 
The prior is a Dirichlet process. 

The data must be nonnegative. We assume \eqn{F(0)=0}. 
}
\details{
The observations must all be non-negative. Due to left truncation,
data must also satisfy \eqn{y_i < x_i}.

}
\references{
Susarla and Van Ryzin (1976)
Nonparametric Bayesian estimation of survival curves from
incomplete observations.
\emph{ J. Amer. Statist. Assoc.} \bold{71}, 
897-902.

Zhou, M. (2000). Nonparametric Bayes estimator of survival functions
for doubly/interval censored data. Tech Report, Univ. of Kentucky.  

See also Zhou, M. (2004). Statistica Sinica. 

Luan, J, and Zhou, M. (2004). 
Nonparametric Bayes estimator of survival functions for
censored data that are also subject to left truncation.
Tech Report. Univ. Kentucky.
}
\author{ Mai Zhou. }
\examples{
trunc <- c(0.2,4,10)
xtime <- c(9,13,15)
d <- c(1,0,0) 
NPBayesT(B=8, theta=0.12, u=3.2, y=trunc, x=xtime, status=d)
#you should get 0.7146325 as the Bayes estimator

ui <- seq(0.1, 20, 0.1)
fui <- ui
for(i in 1:200) 
   fui[i] <- NPBayesT(B=8, theta=0.12, u=ui[i],y=trunc, x=xtime, status=d)
#plot(ui, fui)

ypsy <- c(51, 58, 55, 28, 25, 48, 47, 25, 31, 30, 33, 43, 45, 35, 36)
xpsy <- c(52, 59, 57, 50, 57, 59, 61, 61, 62, 67, 68, 69, 69, 65, 76)
dpsy <- c(1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1 ) 
NPBayesT(B=8,theta=0.02,u=56,y=ypsy,x=xpsy,status=dpsy)
}
\keyword{nonparametric}
\keyword{survival}

