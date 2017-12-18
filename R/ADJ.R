ADJ <-function(X,K=K,n=n,J=J){
  ADJ.mat <- matrix(0,nrow=K,ncol=K)
  ADJ.mat <- t(as.matrix(X))%*%as.matrix(X) / n
  ADJ.mat[!lower.tri(ADJ.mat)] <- NA
  dimnames(ADJ.mat) <- list(J,J)
  return(ADJ.mat)
}
