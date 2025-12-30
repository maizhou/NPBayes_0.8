### NPBayes for truncated and censored data
### GPL copyright (C) 2004-6 Mai Zhou

NPBayesT <- function(B, theta, u, y=numeric(0), x=numeric(0), 
                                           status=numeric(0) ) {
# B and theta are required inputs, they are the parameters for the
# Dirichlet process prior. i.e. the measure \alpha (t, \infty) =
# B \exp( -\theta t). 
# u (required), a scaler, is the time we want to have the Bayes est.
#                    1- \hat F(u) computed.
# y, is the vector of truncation times. 
# x, is the observed (may be right censored) lifetimes .
# status, is the indicator of right censoring.
#
  if(length(u)>1) stop("currently u can only be a scalar")
  if(B <= 0) stop("B must be > 0")
  if(theta <= 0) stop("theta must be > 0")

  fenzi <- NewLemma1(B=B, theta=theta, u=u, y=y, x=x, d=status)
  fenmu <- NewLemma1(B=B, theta=theta, y=y, x=x, d=status)
  return(fenzi/fenmu) 
}

# To do list: (1) same as in NPBayes (2) to save calculation by
# omitting those term that will be canceled in the ratio of fenzi/fenmu.
