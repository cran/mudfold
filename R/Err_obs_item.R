Err_obs_item <-function(X){
  J <- colnames(X)
  O <- Err_obs(X);dimnames(O) <- list(J,J,J)
  f <- colnames(X)
  nn <- length(f)
  cmbf <- combinations(n=nn, r=3, v=f, set=FALSE, repeats.allowed=FALSE)
  ncm <- nrow(cmbf)
  obs <- NULL
  for (i in 1 : nn){
    ind <- NULL;vind <- NULL
    ind <- f[i]
    for (g in 1:ncm){
      if (ind %in% cmbf[g,]){
        vind <- c(vind,g)
      }
    }
    tind <- NULL;Oind <- NULL
    tind <- cmbf[vind,]
    Oind <- sum(O[tind])
    obs <- c(obs,Oind)
  }
  return(obs)
}