CADJ <-function(X,K=K,n=n,J=J){
  CADJ.mat<-matrix(0,nrow=K,ncol=K,dimnames=list(J,J))
  for (i in 1:K) for(j in 1:K) CADJ.mat[i,j] <- sum(X[,i]*X[,j]) / sum(X[,j])
  diag(CADJ.mat) <- NA
  return(CADJ.mat)
}