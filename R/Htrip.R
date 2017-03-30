Htrip <-function(X){
  K = ncol(X)
  n <- nrow(X)
  J <- colnames(X)
  p <- apply(X,2,sum) / n
  EO     <- Err_exp(X);dimnames(EO) <- list(J,J,J)
  O      <- Err_obs(X);dimnames(O) <- list(J,J,J)
  H_hjk  <- array(rep(0,K*K*K), dim=c(K,K,K), dimnames= list(J,J,J))
  for (i in 1 : K) for (j in 1 : K) for(k in 1 : K) {
    H_hjk[i,j,k] <- 1 - O[i,j,k]/EO[i,j,k]
  }
  return(H_hjk)
}
