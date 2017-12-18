DOM <- function(X,K=K,n=n,J=J){
  DOM.mat <- matrix(0,nrow=K,ncol=K,dimnames=list(J,J))
  for (i in 1:K) for (j in 1:K) DOM.mat[i,j] <- sum(X[,i] > X[,j])/n
  diag(DOM.mat) <- NA 
  return(DOM.mat)
}