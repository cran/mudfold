Hscale <-function(X){
  vec <- colnames(X)
  l.vec <-length(vec)
  Hscal<-NULL
  J <-vec
  EO<-Err_exp(X);dimnames(EO) <- list(J,J,J)
  O<-Err_obs(X);dimnames(O) <- list(J,J,J)
  cmb1<-combinations(n=l.vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  Osum<-sum(O[cmb1])
  Esum<-sum(EO[cmb1])
  Hscal<- 1 - Osum / Esum
  return(Hscal)
}
