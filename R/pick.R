pick <- function(x,k=NULL,cutoff=NULL,byItem=FALSE){
  isfactor <- sapply(x,is.factor)
  if(any(isfactor)){
    fac_itm <- which(isfactor)
    dd <- x[fac_itm]
    options(warn=-1)
    xx<-sapply(dd,function(x){as.numeric(as.character(x))})
    options(warn=0)
    # The following deals with the case that the columns are ordered categorical, but not in numerical-like values
    if(sum(is.na(xx))>0){xx<-sapply(dd,as.numeric)}
    x[fac_itm] <- xx
  }
  dims <- dim(x)
  K <- ifelse(byItem,dims[2],dims[1])
  k1 <- is.null(k)
  c1 <- is.null(cutoff)
  kc <- k1 +c1
  if (kc==0) stop("The arguments k and cutoff cannot be used simultaneously. Only one of them can be different than NULL.")
  if (kc==2){
    x <- apply(x,byItem+1,function(col) ifelse(col >= mean(col,na.rm = T),1,0))
    if (!byItem) x <- t(x)
  }
  if (kc==1 & c1){
    x <- apply(x,byItem+1,function(col){
      if(length(col)!=length(unique(as.numeric(col)))){
        or <- order(jitter(as.numeric(col)),decreasing = TRUE)
        col[or][1:k] <- rep(1,k)
        col[or][(k+1):length(or)] <- rep(0,length(col)-k)
      }else{
        or <- order(as.numeric(col),decreasing = TRUE)
        col[or][1:k] <- rep(1,k)
        col[or][(k+1):length(or)] <- rep(0,length(col)-k)
      }
      return(col)
    })
    if (!byItem) x <- t(x)
  }
  if (kc==1 & k1){
    ll <- length(cutoff)
    ll1 <- ll==1
    ll2 <- ll==K
    ll3 <- sum(ll1+ll2)
    if(ll1){
      x <- apply(x,byItem+1, function(col) ifelse(col >= cutoff,1,0))
      if (!byItem) x <- t(x)
    }
    if(ll2){
      if (byItem){
        x <- sapply(1:K, function(col) ifelse(x[,col] < cutoff[col],0,1))
      }else{
        x <- sapply(1:K, function(col) ifelse(x[col,] < cutoff[col],0,1))
        x <- t(x)
      }
      
    }
    if(ll3==0) stop("length(cutoff) must be equal to 1 or to the number of rows/columns of x")
  }
  return(data.frame(x))
} 
  