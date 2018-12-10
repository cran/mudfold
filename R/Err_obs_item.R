Err_obs_item <-function(X,index=NULL,O){
  J <- colnames(X)
  nn <- length(J)
  cmbf <- combinations(n=nn, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  obs <- NULL
  if (is.null(index)){
    for (iter in 1:nn){
      obs <- c(obs,sum(O[cmbf[which(apply(cmbf,1, function(x) J[iter] %in%x)),]]))
    }
  }else{
    obs <- c(obs,sum(O[cmbf[which(apply(cmbf,1, function(x) index %in%x)),]]))
  }
  return(obs)
}