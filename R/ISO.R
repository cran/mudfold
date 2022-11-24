ISO <-function(x, type){
  if (missing(type)) type <-  "item"
  if (!inherits(x, c("mdf","cam.mdf"))) stop("The input of ISO() function should be an output of the CAM() or mudfold() functions respectively.")
  if (!type %in% c("item", "scale")) stop("ISO can be calculated only for items or the total scale.")
  if (inherits(x,"mdf")){
    if (type=="scale") return(x$MUDFOLD_INFO$second_step$ISOscale)
    if (type=="item") return(x$MUDFOLD_INFO$second_step$ISOitem)
  }else{
    K<-ncol(x)
    ISO.mat <- unlist(lapply(1:K, function(i){
      cnd.tmp <- x[i,!is.na(x[i,])]
      xmax <- which(cnd.tmp==max(cnd.tmp))
      lm <- length(xmax)
      if (lm==1){
        ISO_right <- ISO_left <- 0
        if (xmax>=3){
          ISO_lright <- unlist(lapply(1:(xmax-2), function(y) cnd.tmp[y] - cnd.tmp[(y+1):(xmax-1)]),use.names = FALSE)
          ISO_right <- sum(ISO_lright[ISO_lright>0])
        }
        if(xmax <= (K-3)){
          ISO_lleft <- unlist(lapply((K-1):(xmax+2), function(y) cnd.tmp[y] - cnd.tmp[(y-1):(xmax+1)]),use.names = FALSE)
          ISO_left <- sum(ISO_lleft[ISO_lleft>0])
        }
        ISO <- ISO_right+ISO_left
      }else{
        vec.is <- sapply(1:lm, function(w){
          ISO_right <- ISO_left <- 0
          if (xmax[w]>=3){
            ISO_lright <- unlist(lapply(1:(xmax[w]-2), function(y) cnd.tmp[y] - cnd.tmp[(y+1):(xmax[w]-1)]),use.names = FALSE)
            ISO_right <- sum(ISO_lright[ISO_lright>0])
          }
          if(xmax[w]<= (K-3)){
            ISO_lleft <- unlist(lapply((K-1):(xmax[w]+2), function(y) cnd.tmp[y] - cnd.tmp[(y-1):(xmax[w]+1)]),use.names = FALSE)
            ISO_left <- sum(ISO_lleft[ISO_lleft>0])
          }
          return(ISO_right+ISO_left)})
        ISO <- min(vec.is)
      }
      return(ISO)
    }),use.names = FALSE)
    if (type=="item") return(ISO.mat)
    if (type=="scale") return(sum(ISO.mat))
  }
}




