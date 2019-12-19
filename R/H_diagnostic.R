H_diagnostic <- function(mdf, boot, type){
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
    Hcoef <- round(x$MUDFOLD_INFO$second_step$Hitem,3)
    Hcoeftot <- x$MUDFOLD_INFO$second_step$Hscale
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
    missing_indices <- which(is.na(object$BOOTSTRAP$BOOT$t0))
    BOOT_BIAS_STD <- boot_bias_std(object)
    BOOT_BIAS_STD <- cbind(t(sapply(1:tot_cols, 
                                    function(x) imp.moments(object$BOOTSTRAP$BOOT,index = x)$raw )),
                           BOOT_BIAS_STD,iter_used)
    BOOT_BIAS_STD <- round(BOOT_BIAS_STD[,-2],3)
    dimnames(BOOT_BIAS_STD)[[2]] <- c("boot(mean)","boot(bias)","boot(se)","boot(iter)")
    dimnames(BOOT_BIAS_STD)[[1]] <- colnames(object$BOOTSTRAP$BOOT$t)
    BOOTCI <- lapply(c(1:tot_cols), 
                     function(x) boot.ci(object$BOOTSTRAP$BOOT,
                                         index = x,type = type))
    BOOT_BIAS_STD_SCALE <- BOOT_BIAS_STD[scale_in,]
    BOOT_BIAS_STD_SCALE <- BOOT_BIAS_STD_SCALE[1,]
    BOOT_BIAS_STD_ITEMH <- BOOT_BIAS_STD[itemH_in,][match(item.index,sn),]
    
    BOOTCI_SCALE <- BOOTCI[[1]]
    BOOTCI_SCALE[sapply(BOOTCI_SCALE,is.null)] <- NA
    BOOTCI_ITEM_H <- BOOTCI[itemH_in]
    BOOTCI_ITEM_H[sapply(BOOTCI_ITEM_H,is.null)] <- NA
    if (any(is.na(BOOTCI_SCALE[[4]]))) vecH <- c(NA,NA)
    if (type=="norm") vecH <- round(BOOTCI_SCALE$normal[1,2:3],3)
    if (type=="basic") vecH <- round(BOOTCI_SCALE$basic[1,4:5],3)
    if (type=="perc") vecH <- round(BOOTCI_SCALE$percent[1,4:5],3)
    if (type=="bca") vecH <- round(BOOTCI_SCALE$bca[1,4:5],3)
    
    summaryobject$H_SCALE <- rep(NA,7)
    sumScaleH <- c(object$MUDFOLD_INFO$second_step$Hscale,vecH)
    sumScaleH <- round(matrix(sumScaleH,ncol = 3),3)
    dimnames(sumScaleH) <- list("H(scale)",c("value",paste(type,"_lower95CI",sep = ""),paste(type,"_upper95CI",sep = "")))
    summaryobject$H_SCALE <- round(cbind(sumScaleH,t(as.matrix(BOOT_BIAS_STD_SCALE))),3)
    summaryobject$H_ITEM <- list()

    HCISTART <- lapply(BOOTCI_ITEM_H[match(item.index,sn)],function(y){
      if (any(is.na(y[[4]]))){return(c(NA,NA))}else{
        if (type=="norm") return(round(y$normal[,2:3],3))
        if (type=="basic") return(round(y$basic[,4:5],3))
        if (type=="perc") return(round(y$percent[,4:5],3))
        if (type=="bca") return(round(y$percent[,4:5],3))
      }
    })
    sumHit <- data.frame(cbind(Hcoef, round(do.call(rbind,HCISTART),3)))
    dimnames(sumHit)[[2]] <- c("value",paste(type,"_lower95CI",sep=""),paste(type,"_upper95CI",sep = ""))
    summaryobject$H_ITEM <- cbind(sumHit,BOOT_BIAS_STD_ITEMH)
    dimnames(summaryobject$H_ITEM)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMH)[[1]]
    return(summaryobject)
  }else{
    summaryobject <- list()
    summaryobject$H_SCALE <- x$MUDFOLD_INFO$second_step$Hscale
    summaryobject$H_ITEM <- x$MUDFOLD_INFO$second_step$Hitem
    return(summaryobject)
  }
  
}