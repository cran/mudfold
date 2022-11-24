CAM <-function(x){
  if (inherits(x,"mdf")){
    y <- x$MUDFOLD_INFO$second_step$CAM
    class(y) <- "cam.mdf"
    return(y)
  }else{
    J <- colnames(x)
    K <- ncol(x)
    CADJ.mat<-matrix(0,nrow=K,ncol=K,dimnames=list(J,J))
    for (i in 1:K) for(j in 1:K) CADJ.mat[i,j] <- sum(x[,i]*x[,j]) / sum(x[,j])
    diag(CADJ.mat) <- NA
    class(CADJ.mat) <- "cam.mdf"
    return(CADJ.mat)
  }
}