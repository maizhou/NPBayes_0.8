\name{NewLemma1}
\alias{NewLemma1}
\title{
Compute the Conditional Expectation given left truncated and 
   right censored observations
}
\usage{
NewLemma1(B, theta, u=numeric(0), y, x, d)
}
\arguments{
\item{B}{
a positive number. The parameter for the Dirichlet process prior, 
the weight of prior information. If B is very small, then the resulting
prior is "non-informative".
}
\item{theta}{
a positive number. Another parameter for the Dirichlet process prior, 
how quickly the probability decrease.
The measure/parameter is \eqn{ \alpha [t, \infty ) = B \exp( - \theta t)} .
}
\item{u}{ optional. 
a non-negative number, where the expectation of
\eqn{ 1- \hat F(u)} time 
\eqn{ [1-F(x)]/[1-F(y)] } is to be computed.
%\eqn{ 1-F( ) } is to be computed.
}
\item{y}{
vector holding the truncation times.
}
\item{x}{
vector holding the right censored observations.
}
\item{d}{
vectors holding the censoring indicators for x.
}
}
\value{
a single value that is 
the conditional expectation. If \eqn{u} is present then
it includes a term \eqn{ 1- \hat F(u) }.  
If there is uncensored data, then the product it return will
exclude the factor \eqn{ \alpha \{a_i\} }, which is zero under
continuous measure. This do not matter when all we need is the 
ratio of two such terms. They will be canceled in a ratio
expression anyway. (this is the case for the Bayes estimator).
}
\description{
This function will compute the 
expectation of \eqn{ P[u, \infty) \prod P[x_i , \infty)/P[y_i, \infty) },
assuming the distribution function is distributed as a Dirichlet process
prior. (assume \eqn{ x_i } are all right censored.)

Similar to \code{Lemma1} except the term involve \eqn{ y_i }. 

The data must be nonnegative. We assume \eqn{F(0)=0}. 
}
\details{
The parameter of the Dirichlet prior is a measure \eqn{\alpha}:
we took it to be \eqn{\alpha [t, \infty) = B \exp(- \theta t)}. 

If there are uncensored observations \eqn{x}, status=1, 
then the theoretical expectation will include terms 
like \eqn{ \alpha \{ x\} }. Since this is zero for our choice of the 
\eqn{\alpha}, we exclude those factor in the output.

The observations must all be non-negative.

The Bayes estimator is formed with the ratio of two such expectations,
both of them include the term like \eqn{ \alpha \{ x\} }.
}
\references{
Susarla and Van Ryzin (1976)
Nonparametric Bayesian estimation of survival curves from
incomplete observations.
\emph{ J. Amer. Statist. Assoc.} \bold{71}, 
897-902.

Zhou, M. (2000). Nonparametric Bayes estimator of survival functions
for doubly/interval censored data. Tech Report, Univ. of Kentucky.  

Zhou, M. (2004). Nonparametric Bayes estimator of survival functions for interval/doubly
censored data.  Statistica Sinica 14, 533-546.

Luan, J, and Zhou, M. (2005). 
Nonparametric Bayes estimator of survival functions for
censored data that are also subject to left truncation.
Tech Report. Univ. Kentucky.

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

