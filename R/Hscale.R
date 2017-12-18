Hscale <-function(X,vec=vec,EO=EO,O=O){
  l.vec <- length(vec)
  Hscal <- NULL
  cmb1 <- combinations(n=l.vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  Osum <- sum(O[cmb1])
  Esum <- sum(EO[cmb1])
  Hscal <- 1 - (Osum / Esum)
  return(Hscal)
}