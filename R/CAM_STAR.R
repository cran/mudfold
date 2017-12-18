CAM_STAR <- function(X){
  K <- nrow(X)
  J <- colnames(X)
  star <- matrix("",nrow = K,ncol = K,dimnames = list(J,J))
  diag(star) <- "-"
  maxim_pr <- as.numeric(apply(X,1,function(x) max(x,na.rm = T)))
  mlist <- lapply(1:K, function(x) unname(which(X[x,]==maxim_pr[x])))
  for (i in 1:K) star[i,][mlist[[i]]] <- "*"
  return(data.frame(star))
}