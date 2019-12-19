summary_mdf_boot <- function(object, type, ...){
  if(is.null(type)) type <- "perc"
  
  summaryobject <- list()
  if (!type %in%  c("norm","basic", "perc", "bca")) stop("type must be one of: norm / basic / perc / bca ")
  if (type=="stud") stop("studentized intervals cannot be calculated")
  
  dat <- object$CALL$data
  item.index <- object$MUDFOLD_INFO$second_step$scale
  boot.index <- object$BOOTSTRAP$BEST_ORDER
  
  e1 <- all.equal(boot.index,item.index)
  e2 <- all.equal(rev(boot.index),item.index)
  scales_equal <- ifelse((e1[1]==TRUE | e2[1]==TRUE),TRUE, FALSE)
  
  
  si <- object$DESCRIPTIVES$n_items_final
  sn <- object$DESCRIPTIVES$item_names_final
  
  K <- object$MUDFOLD_INFO$second_step$Lscale
  pj<- object$DESCRIPTIVES$prop_per_item[item.index]
  nr <- object$DESCRIPTIVES$n_persons_final
  pj_c <- 1 - pj  
  stdd <- round(sqrt((pj*(1-pj_c))/nr),2)
  fre <- object$DESCRIPTIVES$positives_per_item[item.index]
  Exp.errors<-round(object$MUDFOLD_INFO$second_step$EXPitem, 2)
  Exp.errors.tot <- round(object$MUDFOLD_INFO$second_step$EXPscale,2)
  Obs.errors <- round(object$MUDFOLD_INFO$second_step$OBSitem, 2)
  Obs.errors.tot <- round(object$MUDFOLD_INFO$second_step$OBSscale,2)
  
  Hj <- round(object$MUDFOLD_INFO$second_step$Hitem,2)
  Htot <- round(object$MUDFOLD_INFO$second_step$Hscale,2)
  Iso.stat <- round(object$MUDFOLD_INFO$second_step$ISOitem,3)
  Max.stat <- object$MUDFOLD_INFO$second_step$MAXitem
  
  Iso.stat.tot <- round(object$MUDFOLD_INFO$second_step$ISOscale,2)
  Max.stat.tot <- round(object$MUDFOLD_INFO$second_step$MAXscale,2)
  
  scale_in <- 1:5
  itemH_in <- 6:(5+ncol(dat))
  itemIs_in <- itemH_in + ncol(dat)
  itemMx_in <- itemIs_in + ncol(dat)
  itemEO_in <- itemMx_in + ncol(dat)
  itemO_in <- itemEO_in + ncol(dat)
  tot_cols <- 5+(5*ncol(object$CALL$data))
  iter_used <- object$CALL$nboot - unlist(lapply(1:tot_cols,function(x) sum(is.na(object$BOOTSTRAP$BOOT$t[,x]))))
  object$BOOTSTRAP$BOOT$t <- apply(object$BOOTSTRAP$BOOT$t,2,function(x){
    vi <- x;v <- na.omit(x);lv <- length(v);vv <- unique(v);lvv <- length(vv)
    if (lvv==1){
      val <- ifelse(vv==1,0.99,0.01)
      ind <- sample(1:lv,max(1,round(lv/30)));v[ind] <- val
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
  BOOT_BIAS_STD_ITEMH <- BOOT_BIAS_STD[itemH_in,][match(item.index,sn),]
  BOOT_BIAS_STD_ITEMIS <- BOOT_BIAS_STD[itemIs_in,][match(item.index,sn),]
  BOOT_BIAS_STD_ITEMAX <- BOOT_BIAS_STD[itemMx_in,][match(item.index,sn),]
  BOOT_BIAS_STD_ITEMEO <- BOOT_BIAS_STD[itemEO_in,][match(item.index,sn),]
  BOOT_BIAS_STD_ITEMO <- BOOT_BIAS_STD[itemO_in,][match(item.index,sn),]
  
  
  
  BOOTCI_SCALE <- BOOTCI[scale_in]
  BOOTCI_SCALE[sapply(BOOTCI_SCALE,is.null)] <- NA
  BOOTCI_ITEM_H <- BOOTCI[itemH_in]
  BOOTCI_ITEM_H[sapply(BOOTCI_ITEM_H,is.null)] <- NA
  BOOTCI_ITEM_ISO <- BOOTCI[itemIs_in]
  BOOTCI_ITEM_ISO[sapply(BOOTCI_ITEM_ISO,is.null)] <- NA
  BOOTCI_ITEM_MAX <- BOOTCI[itemMx_in]
  BOOTCI_ITEM_MAX[sapply(BOOTCI_ITEM_MAX,is.null)] <- NA
  BOOTCI_ITEM_EO <- BOOTCI[itemEO_in]
  BOOTCI_ITEM_EO[sapply(BOOTCI_ITEM_EO,is.null)] <- NA
  BOOTCI_ITEM_O <- BOOTCI[itemO_in]
  BOOTCI_ITEM_O[sapply(BOOTCI_ITEM_O,is.null)] <- NA
  
  
  
  
  summaryobject$SCALE_STATS <- NULL
  sumScaleH <- cbind(c(object$MUDFOLD_INFO$second_step$Hscale,
                       object$MUDFOLD_INFO$second_step$ISOscale,
                       object$MUDFOLD_INFO$second_step$MAXscale,
                       object$MUDFOLD_INFO$second_step$EXPscale,
                       object$MUDFOLD_INFO$second_step$OBSscale),
                     t(sapply(BOOTCI_SCALE,function(x){
                       if (any(is.na(x[[4]]))) return(c(NA,NA)) else{
                         if (type=="norm") return(round(x$normal[,2:3],3))
                         if (type=="basic") return(round(x$basic[,4:5],3))
                         if (type=="perc") return(round(x$percent[,4:5],3))
                         if (type=="bca") return(round(x$bca[,4:5],3))
                       } 
                     })))
  sumScaleH <- round(matrix(sumScaleH,ncol = 3),3)
  dimnames(sumScaleH) <- list(c("H(scale)","ISO(scale)","MAX(scale)","EO(scale)","O(scale)"),c("value",paste(type,"_lower95CI",sep = ""),paste(type,"_upper95CI",sep = "")))
  summaryobject$SCALE_STATS <- round(cbind(sumScaleH,BOOT_BIAS_STD_SCALE),3)
  
  
  summaryobject$ITEM_STATS <- list()
  HCISTART <- t(sapply(BOOTCI_ITEM_H[match(item.index,sn)],function(x,type){
    if (any(is.na(x[[4]]))) return(c(NA,NA)) else{
      if (type=="norm") return(round(x$normal[,2:3],3))
      if (type=="basic") return(round(x$basic[,4:5],3))
      if (type=="perc") return(round(x$percent[,4:5],3))
      if (type=="bca") return(round(x$bca[,4:5],3))
    }
  },type=type))
  sumHit <- data.frame(cbind(Hj, round(HCISTART,3)))
  dimnames(sumHit)[[2]] <- c("value",paste(type,"_lower95CI",sep = ""),paste(type,"_upper95CI",sep = ""))
  summaryobject$ITEM_STATS$H_MUDFOLD_items <- cbind(sumHit,BOOT_BIAS_STD_ITEMH)
  dimnames(summaryobject$ITEM_STATS$H_MUDFOLD_items)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMH)[[1]]
  
  
  
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
  summaryobject$ITEM_STATS$ISO_MUDFOLD_items <- cbind(sumISOit,BOOT_BIAS_STD_ITEMIS)
  dimnames(summaryobject$ITEM_STATS$ISO_MUDFOLD_items)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMIS)[[1]]
  
  MAXCISTART <- lapply(BOOTCI_ITEM_MAX[match(item.index,sn)],function(x){
    if (any(is.na(x[[4]]))) return(c(NA,NA)) else{
      if (type=="norm") return(round(x$normal[,2:3],3))
      if (type=="basic") return(round(x$basic[,4:5],3))
      if (type=="perc") return(round(x$percent[,4:5],3))
      if (type=="bca") return(round(x$percent[,4:5],3))
    }
  })
  sumMAXit <- data.frame(cbind(Max.stat, round(do.call(rbind,MAXCISTART),3)))
  dimnames(sumMAXit)[[2]] <- c("value",paste(type,"_lower95CI",sep=""),paste(type,"_upper95CI",sep = ""))
  summaryobject$ITEM_STATS$MAX_MUDFOLD_items <- cbind(sumMAXit,BOOT_BIAS_STD_ITEMAX)
  dimnames(summaryobject$ITEM_STATS$MAX_MUDFOLD_items)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMAX)[[1]]
  
  EOCISTART <- lapply(BOOTCI_ITEM_EO[match(item.index,sn)],function(x){
    if (any(is.na(x[[4]]))) return(c(NA,NA)) else{
      if (type=="norm") return(round(x$normal[,2:3],3))
      if (type=="basic") return(round(x$basic[,4:5],3))
      if (type=="perc") return(round(x$percent[,4:5],3))
      if (type=="bca") return(round(x$percent[,4:5],3))
    }
  })
  sumEOit <- data.frame(cbind(Exp.errors, round(do.call(rbind,EOCISTART),3)))
  dimnames(sumEOit)[[2]] <- c("value",paste(type,"_lower95CI",sep=""),paste(type,"_upper95CI",sep = ""))
  summaryobject$ITEM_STATS$EO_MUDFOLD_items <- cbind(sumEOit,BOOT_BIAS_STD_ITEMEO)
  dimnames(summaryobject$ITEM_STATS$EO_MUDFOLD_items)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMEO)[[1]]
  
  OCISTART <- lapply(BOOTCI_ITEM_O[match(item.index,sn)],function(x){
    if (any(is.na(x[[4]]))) return(c(NA,NA)) else{
      if (type=="norm") return(round(x$normal[,2:3],3))
      if (type=="basic") return(round(x$basic[,4:5],3))
      if (type=="perc") return(round(x$percent[,4:5],3))
      if (type=="bca") return(round(x$percent[,4:5],3))
    }
  })
  sumOit <- data.frame(cbind(Obs.errors, round(do.call(rbind,OCISTART),3)))
  dimnames(sumOit)[[2]] <- c("value",paste(type,"_lower95CI",sep=""),paste(type,"_upper95CI",sep = ""))
  summaryobject$ITEM_STATS$O_MUDFOLD_items <- cbind(sumOit,BOOT_BIAS_STD_ITEMO)
  dimnames(summaryobject$ITEM_STATS$O_MUDFOLD_items)[[1]] <- dimnames(BOOT_BIAS_STD_ITEMO)[[1]]
  
  if (!(object$CALL$Bootstrap & object$CALL$missings == "impute")){
    if (!scales_equal){
      summaryobject$BOOT_SCALE <- NULL
      BOOT_MDF <- as.mudfold(object$CALL$data_s[,boot.index],estimation = object$CALL$estimation)
      sUM_BOOT_MDF <- summary_mdf(BOOT_MDF,diagnostics = FALSE)
      summaryobject$BOOT_SCALE <- sUM_BOOT_MDF
    }
  }
  
  summ <- matrix(NA, nrow=K, ncol=5)
  dimnames(summ)[[2]] <-c("items", "n_persons", 
                          "posit(items)", "pposit(items)", "se(items)" )
  
  summ <- as.data.frame(summ)
  summ[,1] <- item.index
  summ[,2] <- rep(nr,K)
  summ[,3] <- fre
  summ[,4] <- pj
  summ[,5] <- stdd
  summaryobject$ITEM_STATS$ITEM_DESCRIPTIVES <-summ
  return(summaryobject)
}
