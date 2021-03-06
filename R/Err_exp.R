Err_exp <-function(X){
  J <- colnames(X)
  n <- nrow(X)
  K <- ncol(X)
  p <- apply(X,2,sum) / n
  EO<-array(rep(0,K*K*K),dim=c(K,K,K),dimnames=list(J,J,J))
  for (i in 1 : K) for (j in 1 : K) for(k in 1 : K) {
    EO[i,j,k]<- p[i]*(1- p[j])*p[k]*n
  }
  dimnames(EO) <- list(J,J,J)
  return(EO)
}