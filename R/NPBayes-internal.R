### file NPBayes/R/npbayes.R
### copyright (C) 2004-6 Mai Zhou

Allends <- function(lefts, rights) {
#  lefts   are the left end points of intervals,
#  rights  are the right end points of intervals.
if(length(lefts)!=length(rights)) 
                   stop("Lefts and Rights must have same length")
if(any(lefts>=rights)) stop("Rights must > Lefts")
if(length(lefts)>0)
 return(t(1-binary4(length(lefts)))*lefts+t(binary4(length(rights)))*rights)
}

Allsigns <- function(k) {
# generate the signs for the column of Allends().
x1<-c(1,-1)
x2<-c(x1, -x1)
x3<-c(x2, -x2)
x4<-c(x3, -x3)
x5<-c(x4, -x4)
if(k==0) return(1)
if(k==1) return(x1)
if(k==2) return(x2)
if(k==3) return(x3)
if(k==4) return(x4)
if(k==5) return(x5)
if(k>5) {xold<-x5
         num<-5
         while(k>num) {num<-num+1
                       xnew<-c(xold,-xold)
                       xold<-xnew
                      }
         return(xnew)
        }
}

#### This function become standlone 2014.9
#MeasureB <- function(B, theta, ai, zi=numeric(0) ) {
## This function computes the measure beta, as the parameter for Dirichlet
## process prior.
## ai is the partition, any given >0 numbers, that need a measure computed.
## zi is the observed, uncensored data vector.
##
### modified to have more precise outcome. with 120 bits
#     times <- sort(ai)
#	 tivec120 <- mpfr(times, 120)
#	 one120 <- mpfr(1, 120)
#	 zero120 <- mpfr(0, 120)
#	 temp1 <- c(one120, exp(- theta * tivec120), zero120)
#     Malpha <- -B * diff(temp1)
#     if(length(zi)>0) {
#     if(any(is.na(zi))) stop("NA's in the zi?")
#     if(any(zi<0)) stop("negative zi?")
#     temp <- diff(c(length(zi),rowSums(t(outer(zi,times,FUN=">="))),0))
#	 Malpha <- Malpha - mpfr(temp, 120)
#     }
#     return(Malpha)
#}

binary4 <- function(k){ as.matrix( rev(expand.grid(rep(list(0:1),k)))) }

