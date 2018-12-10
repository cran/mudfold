STATSboot <-function(X,z,start,lambda1,lambda2,estimation){
  items <- colnames(X)
  fit <- mudfold(X[z,],start = start,lambda1 = lambda1,lambda2 = lambda2, estimation = estimation,nboot = NULL)
  stats_list_scale <- lapply(vector("list", 5 ),function(x) x <- NA)
  stats_list_Hitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_ISOitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_EOitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  stats_list_Oitems <- lapply(vector("list", length(items)),function(x) x <- NA)
  
  if (!is.null(fit$MUDFOLD_INFO$second_step$scale)){
    vecH <- rep(NA,length(items))
    vecsH <- rep(NA,length(items))
    vecISO <- rep(NA,length(items))
    vecEO <- rep(NA,length(items))
    vecO <- rep(NA,length(items))
    stats_list_scale[[1]] <- paste(fit$MUDFOLD_INFO$second_step$scale,collapse = " ")
    stats_list_scale[[2]] <- fit$MUDFOLD_INFO$second_step$Hscale
    stats_list_scale[[3]] <- fit$MUDFOLD_INFO$second_step$ISOscale
    stats_list_scale[[4]] <- fit$MUDFOLD_INFO$second_step$EXPscale
    stats_list_scale[[5]] <- fit$MUDFOLD_INFO$second_step$OBSscale
    
    mdf_items <- fit$MUDFOLD_INFO$second_step$scale
    ids <- match(mdf_items,items)
    vecH[ids] <- fit$MUDFOLD_INFO$second_step$Hitem
    vecISO[ids] <- fit$MUDFOLD_INFO$second_step$ISOitem
    vecEO[ids] <- fit$MUDFOLD_INFO$second_step$EXPitem
    vecO[ids] <- fit$MUDFOLD_INFO$second_step$OBSitem
    
    for (i in ids){
      stats_list_Hitems[[i]] <- vecH[i]
      stats_list_ISOitems[[i]] <- vecISO[i]
      stats_list_EOitems[[i]] <- vecEO[i]
      stats_list_Oitems[[i]] <- vecO[i]
    }
  }
  stats <- do.call(cbind,
                   c(stats_list_scale,
                     stats_list_Hitems,
                     stats_list_ISOitems,
                     stats_list_EOitems,
                     stats_list_Oitems))
  return(stats)
}
