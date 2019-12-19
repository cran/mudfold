MAX <- function(X,type){
  if (missing(type)) type <-  "item"
  if (!class(X) %in% c("mdf","cam.mdf")) stop("The input of MAX() function should be an output of the CAM() or mudfold() functions respectively.")
  if (!type %in% c("item", "scale")) stop("MAX can be calculated only for items or the total scale.")
  if (class(X)=="mdf"){
    if (type=="scale") return(X$MUDFOLD_INFO$second_step$MAXscale / (X$MUDFOLD_INFO$second_step$Lscale^2 / 12 ))
    if (type=="item") return(X$MUDFOLD_INFO$second_step$MAXitem)
  }else{
    K<-ncol(X)
    max_per_row <- lapply(1:K, function(i){
      mi <- max(X[i,], na.rm =TRUE)
      maxi <- unname(which(X[i,] == mi ))
      checki <- sapply(maxi, function(x)any(c(x+1,x-1)==i))
      if (any(checki))  maxi <- sort(c(i,maxi[min(which(checki))]))
      return(maxi)
    })
    top_down_max <- sapply(1:K, function(i){
      if (i==K) return(0)
      maxi <- max_per_row[[i]]
      list_for_use <- max_per_row[(i+1):K]
      min(sapply(maxi, function(j) sum(sapply( lapply(list_for_use, function(y) min(j-y )), function(w) max(w,0)))))
    })
    bottom_up_max <- sapply(1:K, function(i){
      if (i==1) return(0)
      maxi <- max_per_row[[i]]
      list_for_use <- max_per_row[1:(i-1)]
      min(sapply(maxi, function(mm) sum(sapply( sapply(list_for_use, function(y) min(y-mm )), function(w) max(w,0)))))
    })
    if (type=="scale") if (which.min(c(sum(top_down_max != 0), sum(bottom_up_max != 0))) == 1) return(sum(top_down_max)/(K^2 / 12)) else return(sum(bottom_up_max)/(K^2 / 12))
    if (type=="item") if (which.min(c(sum(top_down_max != 0), sum(bottom_up_max != 0))) == 1) return(top_down_max) else return(bottom_up_max)
  }
}
