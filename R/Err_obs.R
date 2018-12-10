Err_obs<-function(X){
  J <- colnames(X)
  K <- length(J)
  O  <- array(rep(0,K*K*K), dim=c(K,K,K), dimnames= list(J,J,J))
  for (i in 1 : K) for (j in 1 : K) for(k in 1 : K) {
    O[i,j,k] <- sum(X[,i] * (1 - X[,j]) * X[,k])
  }
  dimnames(O) <- list(J,J,J)
  return(O)
}