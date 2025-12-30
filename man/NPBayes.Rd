\name{NPBayes}
\alias{NPBayes}
\title{
Nonparametric Bayes estimate of CDF from arbitrary censored data
}
\usage{
NPBayes(B, theta, u, uncen=numeric(0), rightcen=numeric(0),
          lefts=numeric(0), rights=numeric(0))
}
\arguments{
\item{B}{
a positive number. The parameter for the Dirichlet process prior. 
The weight of prior information. If B is very small, then the resulting
prior is "non-informative".
}
\item{theta}{
a positive number. Another parameter for Dirichlet process prior. 
The measure/parameter is \eqn{ \alpha [t, \infty ) = B \exp( - \theta t)} .
}
\item{u}{
a non-negative number, where the Bayes estimator
\eqn{ 1- \hat F(u)} is to be computed.
}
\item{uncen}{
optional vector holding the uncensored observations.
}
\item{rightcen}{
optional vector holding the right censored observations.
}
\item{lefts}{
optional vectors holding the left end-points of interval 
censored observations.
}
\item{rights}{
optional vectors holding the right end-points of interval 
censored observations. Their length must agree.
}
}
\value{
a single value that is 
the nonparametric Bayes estimator \eqn{ 1- \hat F(u) }.  
}
\description{
This function will compute the nonparametric Bayes estimator of
survival/distribution function from censored data with square error loss. 
The prior is a Dirichlet process. 

The data can be uncensored, right censored, left censored or
interval censored, but must be nonnegative. We assume \eqn{F(0)=0}. 

A left censored observation, \eqn{t}, is handled as an interval censored one
with \code{lefts} = \eqn{0}; \code{rights} = \eqn{t}.

At current, when there are more than 10 interval censored observations, it is slow. 
}
\details{
If \code{uncen}, \code{rightcen}, \code{lefts} and \code{rights}
are all missing, the estimator will be just the prior, 
\eqn{1-\hat F(u) = \exp(-\theta u) }. 

The observations must all be non-negative.

Due to rounding error and loss of significant digits in 
subtraction, the result can be untrustworthy when 
there are many interval censored data.
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
}
\author{ Mai Zhou. }
\examples{
uncensored <- c(1,5,9)
rightcensored <- c(4,7)
NPBayes(B=12, theta=0.2, u=3.2, uncen=uncensored, rightcen=rightcensored)

leftpt <- 0
rightpt <- 3
NPBayes(B=12, theta=0.2, u=3.2, uncen=uncensored, rightcen=rightcensored, 
lefts = leftpt, rights = rightpt)
## the following data runs slowly.
rightcensored <- c(45,37,46,33,15,18,46,46,46,24,46,36,37,22)
leftpt <- c(25,  4, 17,  6,  0,  0, 26, 19, 11, 11, 27)
rightpt <- c(37, 11, 25, 10, 5,  7, 40, 26, 15, 18, 34)
}
\keyword{nonparametric}
\keyword{survival}

