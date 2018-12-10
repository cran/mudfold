Err_exp_item <- function(X,index=NULL,EO){
  J <- colnames(X)
  nn<-length(J)
  cmbf<-combinations(n=nn, r=3, v=J, set=FALSE, repeats.allowed=FALSE)
  expe <- NULL
  if (is.null(index)){
    for (iter in 1:nn){
      expe <- c(expe,sum(EO[cmbf[which(apply(cmbf,1, function(x) J[iter] %in%x)),]]))
    }
  }else{
    expe <- c(expe,sum(EO[cmbf[which(apply(cmbf,1, function(x) index %in%x)),]]))
  }
  return(expe)
}
