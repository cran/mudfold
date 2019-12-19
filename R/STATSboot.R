STATSboot <-function(X,z,start,lambda1,lambda2,estimation,out){
  items <- colnames(X)
  dat <- X[z,]
  if (any(is.na(dat))){
    mice_dat <- data.frame(sapply(dat, as.factor))
    new.pred <- quickpred(mice_dat, method = 'kendall', mincor = out$CALL$mincor)
    mice_imp <- mice(mice_dat,1, method = 'myfunc', printFlag = FALSE, predictorMatrix = new.pred, maxit = 20)
    mdat <- complete(mice_imp,1)
    dat <- data.frame(sapply(mdat,function(y)  as.numeric(levels(y))[y]))
  } 
  dat <- apply(dat, 2, function(x){
    if (any(is.na(x))){
      x[is.na(x)] <- getmode(x)
    }
    x
  })

  fit <- mudfold(dat, start.scale = start,lambda1 = lambda1,lambda2 = lambda2, estimation = estimation,nboot = NULL)
  stats_list_scale <- lapply(vector("list", 6 ),function(x) x <- NA)
  stats_list_Hitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_ISOitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_MAXitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_EOitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_Oitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  
  if (!is.null(fit$MUDFOLD_INFO$second_step$scale)){
    vecH <- rep(NA,length(items))
    vecsH <- rep(NA,length(items))
    vecISO <- rep(NA,length(items))
    vecMAX <- rep(NA,length(items))
    vecEO <- rep(NA,length(items))
    vecO <- rep(NA,length(items))
    stats_list_scale[[1]] <- paste(fit$MUDFOLD_INFO$second_step$scale,collapse = " ")
    stats_list_scale[[2]] <- fit$MUDFOLD_INFO$second_step$Hscale
    stats_list_scale[[3]] <- fit$MUDFOLD_INFO$second_step$ISOscale
    stats_list_scale[[4]] <- fit$MUDFOLD_INFO$second_step$MAXscale
    stats_list_scale[[5]] <- fit$MUDFOLD_INFO$second_step$EXPscale
    stats_list_scale[[6]] <- fit$MUDFOLD_INFO$second_step$OBSscale
    
    mdf_items <- fit$MUDFOLD_INFO$second_step$scale
    ids <- match(mdf_items,items)
    vecH[ids] <- fit$MUDFOLD_INFO$second_step$Hitem
    vecISO[ids] <- fit$MUDFOLD_INFO$second_step$ISOitem
    vecMAX[ids] <- fit$MUDFOLD_INFO$second_step$MAXitem
    vecEO[ids] <- fit$MUDFOLD_INFO$second_step$EXPitem
    vecO[ids] <- fit$MUDFOLD_INFO$second_step$OBSitem
    
    for (i in ids){
      stats_list_Hitems[[i]] <- vecH[i]
      stats_list_ISOitems[[i]] <- vecISO[i]
      stats_list_MAXitems[[i]] <- vecMAX[i]
      stats_list_EOitems[[i]] <- vecEO[i]
      stats_list_Oitems[[i]] <- vecO[i]
    }
  }
  stats <- do.call(cbind,
                   c(stats_list_scale,
                     stats_list_Hitems,
                     stats_list_ISOitems,
                     stats_list_MAXitems,
                     stats_list_EOitems,
                     stats_list_Oitems))
  return(stats)
}
