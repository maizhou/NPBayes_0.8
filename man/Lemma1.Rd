\name{Lemma1}
\alias{Lemma1}
\title{Conditional expectation with respect to a Dirichlet process prior,
       given right censored data.}
\usage{Lemma1(B, theta, ai, zi = numeric(0))}
\arguments{
\item{B}{
a positive number. The parameter for Dirichlet process prior, 
the weight of prior information. If B is very small, then the resulting
Dirichlet process prior is ``non-informative".
}
\item{theta}{
a positive number. Another parameter for Dirichlet process prior. 
The measure/parameter is \eqn{ \alpha [t, \infty ) = B \exp( - \theta t)} .
}
\item{ai}{
vector holding the right censored observations.
}
\item{zi}{
optional vector holding the uncensored observations.
}
}
\value{
a single value that is 
the expectation. 
}
\description{
This function will compute the expectation of \eqn{ \prod P[a_i, \infty) }
with respect to the Dirichlet process prior.

This is the expectation described in Lemma 1 of Zhou (2001), or Zhou (2004). Therefore, the name.
}
\details{
The Dirichlet process prior has a parameter \eqn{ \alpha}, which is a
measure on the positive line. We took this measure \eqn{\alpha} as
\eqn{\alpha [t, \infty) = B \exp ( \theta t ) + 
       \sum I[t \leq z_i \leq \infty)}.

The observations must all be non-negative.
}
\references{
Susarla and Van Ryzin (1976)
Nonparametric Bayesian estimation of survival curves from
incomplete observations.
\emph{ J. Amer. Statist. Assoc.} \bold{71}, 
897-902.

Zhou, M. (2001). Nonparametric Bayes estimator of survival functions
for doubly/interval censored data. Tech Report, Univ. of Kentucky.  

Zhou, M. (2004). Nonparametric Bayes estimator of survival functions for interval/doubly
censored data.  Statistica Sinica 14, 533-546.
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
}
\keyword{nonparametric}
\keyword{survival}

