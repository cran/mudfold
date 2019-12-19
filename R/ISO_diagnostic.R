ISO_diagnostic <- function(mdf, boot, type){
  if (missing(boot)) boot<-FALSE
  if (missing(type)) type<-"perc"
  x <- mdf
  if (boot){
    summaryobject <- list()
    if (!type %in%  c("norm","basic", "perc", "bca")) stop("type must be one of: norm / basic / perc / bca ")
    if (type=="stud") stop("studentized intervals cannot be calculated")
    item.index <- x$MUDFOLD_INFO$second_step$scale
    boot.index <- x$BOOTSTRAP$BEST_ORDER
    dat <- x$CALL$data
    object <- x
    si <- x$DESCRIPTIVES$n_items_final
    sn <- x$DESCRIPTIVES$item_names_final
    K <- object$MUDFOLD_INFO$second_step$Lscale
    Iso.stat <- round(x$MUDFOLD_INFO$second_step$ISOitem,3)
    Iso.stat.tot <- x$MUDFOLD_INFO$second_step$ISOscale
    scale_in <- 1:5
    itemH_in <- 6:(5+ncol(dat))
    itemIs_in <- itemH_in + ncol(dat)
    itemMx_in <- itemIs_in + ncol(dat)
    itemEO_in <- itemMx_in + ncol(dat)
    itemO_in <- itemEO_in + ncol(dat)
    tot_cols <- 5+(5*ncol(x$CALL$data))
    iter_used <- object$CALL$nboot - unlist(lapply(1:tot_cols,function(x) sum(is.na(object$BOOTSTRAP$BOOT$t[,x]))))
    object$BOOTSTRAP$BOOT$t <- apply(object$BOOTSTRAP$BOOT$t,2,function(x){
      vi <- x;v <- na.omit(x);lv <- length(v);vv <- unique(v);lvv <- length(vv)
      if (lvv==1){
        val <- ifelse(vv==1,0.99,0.01)
        ind <- sample(1:lv,max(1,round(lv/30)))
        v[ind] <- val
        vi[!is.na(vi)] <-v
      }
      return(vi)
    })
    BOOT_BIAS_STD <- boot_bias_std(object)
    BOOT_BIAS_STD <- cbind(t(sapply(1:tot_cols, 
                                    function(x) imp.moments(object$BOOTSTRAP$BOOT,index = x)$raw )),
                           BOOT_BIAS_STD,iter_used)
    BOOT_BIAS_STD <- round(BOOT_BIAS_STD[,-2],3)
    dimnames(BOOT_BIAS_STD)[[2]] <- c("boot(mean)","boot(bias)","boot(se)","boot(iter)")
    dimnames(BOOT_BIAS_STD)[[1]] <- colnames(object$BOOTSTRAP$BOOT$t)
    BOOTCI <- lapply(1:tot_cols, 
                     function(x) boot.ci(object$BOOTSTRAP$BOOT,
                                         index = x,type = type))
    
    BOOT_BIAS_STD_SCALE <- BOOT_BIAS_STD[scale_in,]
    BOOT_BIAS_STD_SCALE <- BOOT_BIAS_STD_SCALE[2,]
    BOOT_BIAS_STD_ITEMIS <- BOOT_BIAS_STD[itemIs_in,][match(item.index,sn),]
    
    BOOTCI_SCALE <- BOOTCI[[2]]
    BOOTCI_SCALE[sapply(BOOTCI_SCALE,is.null)] <- NA
    BOOTCI_ITEM_ISO <- BOOTCI[itemIs_in]
    BOOTCI_ITEM_ISO[sapply(BOOTCI_ITEM_ISO,is.null)] <- NA
    
    
    if (any(is.na(BOOTCI_SCALE[[4]]))) vectISO <- c(NA,NA)
    if (type=="norm") vectISO <- round(BOOTCI_SCALE$normal[1,2:3],3)
    if (type=="basic") vectISO <- round(BOOTCI_SCALE$basic[1,4:5],3)
    if (type=="perc") vectISO <- round(BOOTCI_SCALE$percent[1,4:5],3)
    if (type=="bca") vectISO <- round(BOOTCI_SCALE$bca[1,4:5],3)
    
    summaryobject$ISO_SCALE <- NULL
    sumScaleH <- c(object$MUDFOLD_INFO$second_step$ISOscale,vectISO)
    sumScaleH <- round(matrix(sumScaleH,ncol = 3),3)
    dimnames(sumScaleH) <- list("ISO(scale)",c("value",paste(type,"_lower95CI",sep = ""),paste(type,"_upper95CI",sep = "")))
    summaryobject$ISO_SCALE <- round(cbind(sumScaleH,t(as.matrix(BOOT_BIAS_STD_SCALE))),3)
    
    summaryobject$ISO_ITEM <- list()
  
    ISOCISTART <- lapply(BOOTCI_ITEM_ISO[match(item.index,sn)],function(x){
      if (any(is.na(x[[4]]))) return(c(NA,NA)) else{
        if (type=="norm") return(round(x$normal[,2:3],3))
        if (type=="basic") return(round(x$basic[,4:5],3))
        if (type=="perc") return(round(x$percent[,4:5],3))
        if (type=="bca") return(round(x$percent[,4:5],3))
      }
    })
    sumISOit <- data.frame(cbind(Iso.stat, round(do.call(rbind,ISOCISTART),3)))
    dimnames(sumISOit)[[2]] <- c("value",paste(type,"_lower95CI",sep=""),paste(type,"_upper95CI",sep = ""))
    summaryobject$ISO_ITEM <- cbind(sumISOit,BOOT_BIAS_STD_ITEMIS)
    dimnames(summaryobject$ISO_ITEM)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMIS)[[1]]
    return(summaryobject)
  }else{
    summaryobject <- list()
    summaryobject$ISO_SCALE <- x$MUDFOLD_INFO$second_step$ISOscale
    summaryobject$ISO_ITEM <- x$MUDFOLD_INFO$second_step$ISOitem
    return(summaryobject)
  }
  
}