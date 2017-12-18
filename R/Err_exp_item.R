Err_exp_item <- function(X,K=K,n=n,J=J,EO=EO){
  nn<-length(J)
  cmbf<-combinations(n=nn, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  ncm<-nrow(cmbf)
  expe<-vector()
  for (i in 1 : nn){
    ind <- NULL;vind <- NULL
    ind <- J[i]
    for (g in 1:ncm){
      if (ind %in% cmbf[g,]){
        vind <- c(vind,g)
      }
    }
    tind <- NULL;EOind <- NULL
    tind <- cmbf[vind,]
    EOind <- sum(EO[tind])
    expe <- c(expe,EOind)
  }
  return(expe)
}