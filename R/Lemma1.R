Lemma1 <- function(B, theta, ai, zi=numeric(0)) {
          if(B<=0) stop("weight B must be > 0")
          if(theta<=0) stop("theta must be > 0")
          if(any(ai<0)) stop("partition ai must be >= 0")
          if(all(ai==0)) return( mpfr(1, 120) )
             else { ai <- ai[ai>0]
                    BR <- B + length(zi)
                    N <- length(ai)-1
                    fmu <- BR + (0:N)
                    Bi <- MeasureB(B, theta, ai, zi)
                    fz <- (0:N) + cumsum(rev(Bi))[1:(N+1)]
                    return( prod(fz/fmu) )
                  }
}

