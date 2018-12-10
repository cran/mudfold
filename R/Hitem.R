Hitem <- function(X,index=NULL,EO,O){
  J <- colnames(X)
  nn <- length(J)
  cmbf <- combinations(n=nn, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  Hj <- NULL
  if (is.null(index)){
    for (iter in 1:nn){
      persi <- cmbf[which(apply(cmbf,1, function(x) J[iter] %in% x)),]
      Hj <- c(Hj,1 - sum(O[persi]) / sum(EO[persi]))
    }
  }else{
    persi <- cmbf[which(apply(cmbf,1, function(x) index %in% x)),]
    Hj <- c(Hj,1 - sum(O[persi]) / sum(EO[persi]))
  }
  return(Hj)
}