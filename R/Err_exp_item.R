Err_exp_item<-function(X){
  J<-colnames(X)
  EO<- Err_exp(X);dimnames(EO) <- list(J,J,J)
  f<-colnames(X)
  nn<-length(f)
  cmbf<-combinations(n=nn, r=3, v=f, set=FALSE, repeats.allowed=FALSE)
  ncm<-nrow(cmbf)
  expe<-vector()
  for (i in 1 : nn){
    ind <- NULL;vind <- NULL
    ind <- f[i]
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
