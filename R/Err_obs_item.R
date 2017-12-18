Err_obs_item <-function(X,K=K,n=n,J=J,O=O){
  nn <- length(J)
  cmbf <- combinations(n=nn, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  ncm <- nrow(cmbf)
  obs <- NULL
  for (i in 1 : nn){
    ind <- NULL;vind <- NULL
    ind <- J[i]
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