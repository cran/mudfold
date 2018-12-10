Hscale <-function(X,EO,O){
  J <- colnames(X)
  nn <- length(J)
  Hscal <- NULL
  cmb1 <- combinations(n=nn, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  Osum <- sum(O[cmb1])
  Esum <- sum(EO[cmb1])
  Hscal <- 1 - (Osum / Esum)
  return(Hscal)
}