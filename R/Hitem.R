Hitem <-function(X){
  strt.names <- colnames(X)
  K = ncol(X)
  n <- nrow(X)
  J <- colnames(X)
  EO<- Err_exp(X);dimnames(EO) <- list(J,J,J)
  O <- Err_obs(X);dimnames(O) <- list(J,J,J)
  f <- J
  cmbf <- combinations(n=K, r=3, v=f, set=FALSE, repeats.allowed=FALSE)
  ncm <- nrow(cmbf)
  Hj <- NULL
  for (i in 1 : K){
    ind <- NULL;vind <- NULL
    ind <- f[i]
    for (g in 1:ncm){
      if (ind %in% cmbf[g,]){
        vind <- c(vind,g)
      }
    }
    tind <- NULL;EOind <- NULL;Oind <- NULL;Hind <-NULL
    tind <- cmbf[vind,]
    Oind <- sum(O[tind])
    EOind <- sum(EO[tind])
    Hind <- 1 - Oind / EOind
    Hj <- c(Hj,Hind)
  }
  return(Hj)
}