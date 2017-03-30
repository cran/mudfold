ADJ <-function(X){
  K = ncol(X)
  n <- nrow(X)
  J <- colnames(X)
  ADJ.mat <- matrix(0,nrow=K,ncol=K)
  ADJ.mat <- t(as.matrix(X))%*%as.matrix(X) / nrow(X)
  ADJ.mat[!lower.tri(ADJ.mat)] <- NA
  dimnames(ADJ.mat) =list(J,J)
  return(ADJ.mat)
}