Err_obs<-function(X){
  K = ncol(X)
  n <- nrow(X)
  J <- colnames(X)
  O <- array(rep(0,K*K*K), dim=c(K,K,K), dimnames= list(J,J,J))
  for (i in 1 : K) for (j in 1 : K) for(k in 1 : K) {
    O[i,j,k] <- sum(X[,i] * (1 - X[,j]) * X[,k])
  }
  return(O)
}