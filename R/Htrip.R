Htrip <-function(X,K=K,n=n,J=J,p=p,EO=EO,O=O){
  H_hjk  <- array(rep(0,K*K*K), dim=c(K,K,K), dimnames= list(J,J,J))
  for (i in 1 : K) for (j in 1 : K) for(k in 1 : K) {
    H_hjk[i,j,k] <- 1 - O[i,j,k]/EO[i,j,k]
  }
  return(H_hjk)
}