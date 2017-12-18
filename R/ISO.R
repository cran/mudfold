ISO <-function(X){
  ca.dfa<-X
  J<-colnames(X)
  K<-ncol(X)
  cnd.adj <- X
  n<-nrow(cnd.adj)
  cnd.tmp<-t(matrix(t(cnd.adj)[-seq(1,n^2,n+1)], n-1, n))
  nn <- (n-1)
  xmax <- apply(cnd.tmp,1,function(x) which(x==max(x)))
  lm<-sapply(xmax, length)
  ISO.mat <- rep(0,K)
  for (i in 1:n){
    
    if (lm[[i]]==1){
      if (xmax[[i]]>=3){
        for (j in 1:(xmax[[i]]-2)){
          for ( k in (j+1):(xmax[[i]]-1) ){
            ISO.mat[i]=ISO.mat[i]+max(cnd.tmp[i,j]-cnd.tmp[i,k],0)
          }
        }
      }
      if(xmax[[i]]<= nn-2){
        for(j in nn:(xmax[[i]]+2)){
          for(k in (j-1):(xmax[[i]]+1)){
            ISO.mat[i]=ISO.mat[i]+max(cnd.tmp[i,j]-cnd.tmp[i,k],0)
          }
        }
      }
    }
    
    if (lm[[i]]>1){
      vec.is<-rep(0,lm[[i]])
      for (w in 1:lm[[i]]){
        if (xmax[[i]][w]>=3){
          for (j in 1:(xmax[[i]][w]-2)){
            for ( k in (j+1):(xmax[[i]][w]-1) ){
              vec.is[w]<-vec.is[w]+max(cnd.tmp[i,j]-cnd.tmp[i,k],0)
            }
          }
        }
        if(xmax[[i]][w]<= nn-2){
          for(j in nn:(xmax[[i]][w]+2)){
            for(k in (j-1):(xmax[[i]][w]+1)){
              vec.is[w]<-vec.is[w]+max(cnd.tmp[i,j]-cnd.tmp[i,k],0)
            }
          }
        }
      }
      ISO.mat[i]<-vec.is[which.min(vec.is)]
    }
  }
  return(ISO.mat)
}