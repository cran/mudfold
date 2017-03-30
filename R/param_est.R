param_est<-function(X,method=c("rank","quantile")){
  if (sum(method %in% c("rank","quantile"))==1){
    p <- ncol(X)
    n <- nrow(X)
    totscore <- apply(X, 1,sum)
    if (method=="quantile"){
      r <- 1:p
      q <- r/ p
      Tqj <- NULL
      for (l in 1:n){
        Tqj[l] <- sum(q*X[l,]) /  totscore[l]
      }
      Tqj[is.nan(Tqj)] <-NA
      names(q) <- colnames(X)
      out<-list(betas=q,thetas=Tqj)
    }else{
      r <- 1:p
      Tqj <- NULL
      for (l in 1:n){
        Tqj[l] <- sum(r*X[l,]) /  totscore[l]
        Tqj[is.nan(Tqj)] <-NA
        names(r) <- colnames(X)
        out<-list(betas=r,thetas=Tqj)
      }
    }
    
    return(out)
  }else{
    if (sum(method %in% c("rank","quantile"))==0) print("Provide a valid estimation method.")
    if (sum(method %in% c("rank","quantile"))==2) print("Select one of the two estimation methods.")
  }
}
