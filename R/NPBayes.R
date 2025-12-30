### file NPBayes/R/npbayes.R
### copyright (C) 2004-6 Mai Zhou

NPBayes <- function(B, theta, u, uncen=numeric(0), rightcen=numeric(0),
                                   lefts=numeric(0), rights=numeric(0)) {
# B and theta are required inputs, they are the parameters for the
# Dirichlet process prior. i.e. the measure \alpha (t, \infty) =
# B \exp( -\theta t). 
# u (required), a scaler, is the time we want to have 1-F(u) computed.
# uncen, rightcen are the uncensored and right censored observations.
# lefts, rights are the end points of interval censored observations.
# If there are no uncensored observations we should say uncen=numeric(0) etc.
# The left censored observations can be handled by treating them as
# interval censored: [0, a) == left censored at a.
#
# Due to rounding errors and loss of significant digits, if there are a lot
# of interval censored data, the result can be untrustworthy.

  if(length(u)>1) stop("currently u can only be scalar")
  matends <- Allends(lefts=lefts, rights=rights)
  dimnames(matends) <- NULL
  vecsigns <- Allsigns(length(lefts))
  M <- length(vecsigns)
  numer <- denom <- rep(0, M)
  numer <- mpfr(numer, 120)
  denom <- mpfr(denom, 120)
 for(i in 1:M) {
    partit <- c(rightcen, matends[,i])
    numer[i] <- Lemma1(B=B, theta=theta, ai=c(u,partit), zi=uncen)
    denom[i] <- Lemma1(B=B, theta=theta, ai=partit, zi=uncen)
    }
  return(sum(numer*vecsigns)/sum(denom*vecsigns))
}

# To do list: (1) To make the choice of alpha measure more flexible.
# Instead of the type B exp(-theta t), let the user supply their
# own measure. (2) To remove the restriction of non-negative data.
# so the alpha measure can be on the whole line instead of positive line.
