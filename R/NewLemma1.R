### copyright (C) 2004 Mai Zhou 
# Compute an expectation of a likelihood. 
# B and theta are required inputs, they are the parameters for the
# Dirichlet process prior. i.e. the measure \alpha (t, \infty) =
# B \exp( -\theta t). 
# y is the (left) truncation times.
# x is the (possibly) right censored observations.
# d is the censoring status.
#   if there is a u, then the likelihood include a term P([u,\infty)).

NewLemma1 <- function(B, theta, u = numeric(0), y, x, d) {
if(B<=0) stop("weight B must be > 0")
if(theta<=0) stop("theta must be > 0")
n <- length(y)
if(length(x) != n) stop("length of x and y must agree")
if(length(d) != n) stop("length of d and y must agree")
if(any((d!=0)&(d!=1))) stop("d must be 0/1's for censor/not-censor")
if( any(x <= y) ) stop("x must > y, as y is the entry time")

if(length(u) == 0) 
mydata <- matrix(c(y,x,rep(-1,n),rep(1,n),rep(9,n),d), 
                            nrow=3, ncol=2*n, byrow=TRUE)

if(length(u) == 1) 
mydata <- matrix(c(u,y,x, 1,rep(-1,n),rep(1,n), 0,rep(9,n),d), 
                            nrow=3, ncol=(2*n +1), byrow=TRUE)

if(length(u) > 1) stop("u can only be a scalar")
zorder <- order(mydata[1, ])
sortdata <- mydata[ ,zorder]
## first row is the times y and x. Second row is the power in Likelihood.
## Third row is the censoring indicator, 0=right censor, 1=not censor.
if(all(sortdata[1,]==0)) return(1) 
if(any(sortdata[1,] < 0)) stop("y,x must > 0")
else { ns <- rev(cumsum(rev(sortdata[2,])))
       ns <- c(ns[-1], 0) 
       ms <- as.numeric(sortdata[2,]==-1)
       Bi <- B * exp(- theta * sortdata[1,]) 
       fz <- ( Bi + ns - ms )^sortdata[2,]
       fz <- fz[sortdata[3,] != 1] 
       fz <- prod(fz)
       if(length(u) == 1) fz <- fz/B
       return(fz)
      }
}

