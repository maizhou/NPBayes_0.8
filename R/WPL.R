WPL <- function(x,d,w=rep(1, length(d)),y=rep(-Inf, length(d))) {
#### For Left Truncated and Right Censored data, with weights.
#### This is actually Lynden-Bell or WJT estimator, with
#### weights. If all y are -Inf, (no left truncation) this
#### reduces to the Kaplan-Meier/Nelson Aalen est. for right
#### censor only data.  (similar to LTRC( ) in package emplik.)

temp <- Wdataclean2(z=x,d=d,wt=w)
dd <- temp$dd
ww <- temp$weight
dd[length(dd)] <- 1
xx <- temp$value

temp <- DnR(xx,dd,ww,y=y)

NelAal <- temp$n.event/temp$n.risk
survP <- cumprod( 1 - NelAal )
NelAal <- cumsum(NelAal)
jumps <- -diff( c(1, survP) )

list(times=xx[dd==1], survjump=jumps, surv=survP, CumHaz=NelAal)
}

Wdataclean2 <- function(z, d, wt = rep(1,length(z)) ) 
{  niceorder <- order(z,-d)
   sortedz <- z[niceorder]
   sortedd <- d[niceorder]
   sortedw <- wt[niceorder]

   n <- length(sortedd)
   y1 <- sortedz[-1] != sortedz[-n]
   y2 <- sortedd[-1] != sortedd[-n]
   y <- y1 | y2

   ind <- c(which(y | is.na(y)), n)

   csumw <- cumsum(sortedw)

   list( value = sortedz[ind], dd = sortedd[ind],
         weight = diff(c(0, csumw[ind])) )
}
##############################################################
# this function sorts the data, and collaps them
# if there are true tie. and number of tie counted in weight
##############################################################

DnR <- function(x, d, w, y=rep(-Inf, length(x)) )
{
## inputs should be from Wdataclean2(), i.e. ordered and
## weighted.
## y is the truncation times, y do not have to be the same
## length as x, but should be length(y) = sum(w).

allrisk <- rev(cumsum(rev(w)))
posi <- d == 1
uncenx <- x[posi]
uncenw <- w[posi]
uncenR <- allrisk[posi]

if(any(y > -Inf)) { 
  inde <- function(u, v) { as.numeric(u >= v) }
  uuij <- outer(y, uncenx, FUN="inde")
  trunca <- as.vector( rowsum( uuij, group= rep(1, length(y))) ) 
  uncenR <- uncenR - trunca
}

list( times = uncenx, n.risk = uncenR, n.event = uncenw )
}