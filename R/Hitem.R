Hitem <-function(X,K=K,n=n,J=J,EO=EO,O=O){
  cmbf <- combinations(n=K, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  ncm <- nrow(cmbf)
  Hj <- NULL
  for (i in 1 : K){
    ind <- NULL;vind <- NULL
    ind <- J[i]
    for (g in 1:ncm){
      if (ind %in% cmbf[g,]){
        vind <- c(vind,g)
      }
    }
    tind <- NULL;EOind <- NULL;Oind <- NULL;Hind <-NULL
    tind <- cmbf[vind,]
    Oind <- sum(O[tind])
    EOind <- sum(EO[tind])
    Hind <- 1 - (Oind / EOind)
    Hj <- c(Hj,Hind)
  }
  return(Hj)
}