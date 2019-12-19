mice_mudfold_noboot <- function(out){
  out_list <- out
  nmice <- out_list$CALL$nmice
  data <- out_list$CALL$data
  I <- colnames(data)
  nrdat <- nrow(data)
  for_mice_dat <- data.frame(sapply(data,as.factor))
  new.pred <- quickpred(for_mice_dat, method = 'kendall', mincor = out_list$CALL$mincor)
  imp_dati <- mice(for_mice_dat, m = nmice,predictorMatrix = new.pred, printFlag = FALSE, method = 'myfunc')
  dims_imp_dati <- lapply(imp_dati$imp,dim)
  index_which_imputed <- unlist(lapply(dims_imp_dati, function(x) ifelse(x[1] > 0, TRUE, FALSE)))
  mode_imputed <- lapply(imp_dati$imp[index_which_imputed], function(x) apply(x, 1, getmode))
  dat_imputed <- data[,index_which_imputed]
  dat_imputed <- data.frame(do.call(cbind,lapply(1:sum(index_which_imputed), function(i){
    y <- dat_imputed[,i]
    y[is.na(y)] <- mode_imputed[[i]]
    y
  })))
  dt <- data
  dt[,index_which_imputed] <- dat_imputed
  out_list$CALL$mode_mice_data <- dt
  dati <- complete(imp_dati, "all")
  dati <- lapply(dati, function(x){
    if (any(is.na(x))){
      x <- apply(x, 2, function(y){
        if(any(is.na(y))) y[is.na(y)] <- getmode(y)
      })
    }
    x
  })
  
  dati <- lapply(dati, function(x) data.frame(sapply(x, function(y) as.numeric(levels(y))[y])))
  
  check_results <- lapply(dati, function(x){
    out_new <- out_list
    out_new$CALL$data <- x
    data_check(out_new)})
  nullvec <- sapply(check_results, is.null)
  if (any(nullvec)) return()
  
  list_res <- list()
  list_res <- out_list
  list_res$MUDFOLD_INFO <- list()
  list_res$MUDFOLD_INFO$triple_stats <- list()
  list_res$MUDFOLD_INFO$triple_stats$Expected_errors <- Reduce("+", lapply(check_results, function(x) x$out$MUDFOLD_INFO$triple_stats$Expected_errors )) / nmice
  list_res$MUDFOLD_INFO$triple_stats$Observed_errors <- Reduce("+", lapply(check_results, function(x) x$out$MUDFOLD_INFO$triple_stats$Observed_errors )) / nmice 
  list_res$MUDFOLD_INFO$triple_stats$H_coefficients <- Reduce("+", lapply(check_results, function(x) x$out$MUDFOLD_INFO$triple_stats$H_coefficients )) / nmice 
  
  out_lists <- lapply(check_results, function(x) x$out)
  out_lists <- lapply(1:length(dati), function(x){
    out_lists[[x]]$CALL$data <-check_results[[x]]$data
    out_lists[[x]]
  })
  fits <- lapply(1:nmice, function(i){
    if (is.null(out_list$CALL$start.scale)){
      res_first <- mudfold_fs(out_lists[[i]])
      res_second <- mudfold_se(res_first)
    }else{
      res_first <- out_lists[[i]]
      res_first$MUDFOLD_INFO$first_step <- NULL
      res_second <- mudfold_se(res_first)
    }
    return(res_second)
  })
  non_conv <- unlist(lapply(fits, is.null))
  conv <- !non_conv
  nconvergence <- sum(conv)
  convergence_rate <- (nconvergence )/ nmice
  list_res$MUDFOLD_INFO$mice_info <- list()
  list_res$MUDFOLD_INFO$mice_info$mice_out <- imp_dati
  list_res$MUDFOLD_INFO$mice_info$convergence <- nconvergence
  list_res$MUDFOLD_INFO$mice_info$convergence_rate <- convergence_rate
  list_res$MUDFOLD_INFO$mice_info$which_non_convergence <- which(non_conv)
  list_res$MUDFOLD_INFO$first_step <- list()
  list_res$MUDFOLD_INFO$second_step <- list()
  
  if (nconvergence > 0){
    list_res$MUDFOLD_INFO$first_step$Converged <- TRUE
    list_res$MUDFOLD_INFO$second_step$Converged <- TRUE
  }else{
    print(paste("NO MUDFOLD SCALE OBTAINED IN ",nmice," MICE DATASETS",sep=""))
    return()
  }   
  nmice_effective <- c(1:nmice)[conv]

  summ_fits <- lapply(fits[nmice_effective], summary)
  ## Find the best unique triple that appeared most often in m mice datasets
  Uniq_trip <- lapply(fits[nmice_effective], function(x) as.vector(x$MUDFOLD_INFO$first_step$BU))
  for( i in 1:nconvergence){
    ut1 <- paste(Uniq_trip[[i]],collapse = "")
    ut2 <- paste(rev(Uniq_trip[[i]]),collapse = "")
    equal_trips <- which(sapply(Uniq_trip, function(x) paste(x,collapse = "") == ut1 |  paste(x,collapse = "") == ut2))
    for (j in equal_trips){
      Uniq_trip[[j]] <- Uniq_trip[[i]]
    }
  }
  Uniq_trip <- sapply(Uniq_trip, function(x) paste(x,collapse = ""))
  ttrips <- table(Uniq_trip)
  best_mice_utrip <- names(ttrips)[which.max(ttrips)]
  ref_fit <- which(Uniq_trip == best_mice_utrip)[1]
  best_mice_utrip <- fits[[ref_fit]]$MUDFOLD_INFO$first_step$BU
  Htrips <- sapply(names(ttrips), function(u){
    z<-fits[nmice_effective]
    zstar <- z[which(Uniq_trip == u)]
    utrip_H <- mean(sapply(zstar, function(x) x$MUDFOLD_INFO$first_step$HBU))
    utrip_H
  })

  fits_all <- fits[nmice_effective]
  which_fits <- which(sapply(fits_all, function(x){
    sum(c(x$MUDFOLD_INFO$first_step$BU)==best_mice_utrip) == 3 | sum(c(x$MUDFOLD_INFO$first_step$BU) == rev(best_mice_utrip))==3
  }))
  fits1 <- fits_all[which_fits]
  utrip_H <- mean(sapply(fits1, function(x) x$MUDFOLD_INFO$first_step$HBU))
  list_res$MUDFOLD_INFO$first_step$unique_triples <- sort(ttrips,decreasing = TRUE)
  list_res$MUDFOLD_INFO$first_step$H_unique_triples <- Htrips
  list_res$MUDFOLD_INFO$first_step$BU <- best_mice_utrip
  list_res$MUDFOLD_INFO$first_step$HBU <- utrip_H

  ## Find the scale that appeared most often in m mice datasets
  scales <- lapply(summ_fits, function(x) x$ITEM_STATS$items)
  for( i in 1:nconvergence){
    sc1 <- paste(scales[[i]],collapse = "")
    sc2 <- paste(rev(scales[[i]]),collapse = "")
    equal_scales <- which(sapply(scales, function(x) paste(x,collapse = "") ==sc1 |  paste(x,collapse = "")==sc2))
    for (j in equal_scales){
      scales[[j]] <- scales[[i]]
    }
  }
  scalescol <- sapply(scales, function(x) paste(x,collapse = ""))
  tscales <- table(scalescol)
  index_scales <-  as.numeric(which(tscales== max(tscales))) 
  names_ind_scales <- names(tscales)[index_scales]
  list_res$MUDFOLD_INFO$second_step$mice_scales <- tscales
  best_scale <- NA
  if (length(index_scales)==1){
    best_scale <- names_ind_scales
  } 
  if (length(index_scales) > 1){
    name_vec_new <- names_ind_scales
    meanISO <- sapply(name_vec_new, function(x){
      z <- fits_all[which(scalescol == x)] 
      mean(sapply(z, function(y) y$MUDFOLD_INFO$second_step$ISOscale))
    })
    indexes2 <- as.numeric(which(meanISO == min(meanISO)))
    if (length(indexes2) == 1){
      best_scale <- names(meanISO)[indexes2]
    } 
    if (length(indexes2) > 1){
      name_vec_new <- name_vec_new[indexes2]
      meanMAX <- sapply(name_vec_new, function(x){
        z <- fits_all[which(scalescol == x)] 
        mean(sapply(z, function(y) y$MUDFOLD_INFO$second_step$MAXscale))
      })
      indexes3 <- which(meanMAX == min(meanMAX))
      if (length(indexes3) == 1){
        best_scale <- names(meanMAX)[indexes3]
      } 
      if (length(indexes3) > 1){
        name_vec_new <- name_vec_new[indexes3]
        meanH <- sapply(names_ind_scales[indexes3], function(x){
          z <- fits_all[which(scalescol == x)] 
          mean(sapply(z, function(y) y$MUDFOLD_INFO$second_step$Hscale))
        })
        indexes4 <- which(meanH == max(meanH))
        best_scale <- names(meanH)[1]
      }
    }
  }
  refscale2 <- which(scalescol == best_scale)[1]
  
  best_mice_scale <- fits_all[[refscale2]]$MUDFOLD_INFO$second_step$scale
  length_best_mice_scale <- length(best_mice_scale)
  which_summary_to_use <- which(sapply(summ_fits, function(x){
    if (length(x$ITEM_STATS$items) == length_best_mice_scale){
      sum(x$ITEM_STATS$items==best_mice_scale)==length_best_mice_scale | sum(x$ITEM_STATS$items==rev(best_mice_scale))==length_best_mice_scale
    }else{
      FALSE
    }
  }))
  summ_fits <-  summ_fits[which_summary_to_use]
  scale_stats <- apply(do.call(rbind,lapply(summ_fits, function(x) c(x$SCALE_STATS)$value)),2,mean)
  item_stats <- Reduce("+",lapply(summ_fits, function(x){
    sc1c <- sum(x$ITEM_STATS$items == rev(best_mice_scale)) == length_best_mice_scale
    sc2c <- sum(x$ITEM_STATS$items == best_mice_scale) == length_best_mice_scale
    nrx <- nrow(x$ITEM_STATS)
    if (sc1c) x$ITEM_STATS <- x$ITEM_STATS[rev(1:nrx),]
    unlist(x$ITEM_STATS[,-c(1,2)])
  })) / length(which_summary_to_use)
  
  av_CAM <- Reduce("+",lapply(fits_all[which_summary_to_use], function(x) x$MUDFOLD_INFO$second_step$CAM[best_mice_scale,best_mice_scale])) /length(which_summary_to_use)
  av_COR <- Reduce("+",lapply(fits_all[which_summary_to_use], function(x) x$MUDFOLD_INFO$second_step$CORR[best_mice_scale,best_mice_scale])) /length(which_summary_to_use)
  av_ADJ <- Reduce("+",lapply(fits_all[which_summary_to_use], function(x) x$MUDFOLD_INFO$second_step$ADJ[best_mice_scale,best_mice_scale])) /length(which_summary_to_use)
  av_DOM <- Reduce("+",lapply(fits_all[which_summary_to_use], function(x) x$MUDFOLD_INFO$second_step$DOM[best_mice_scale,best_mice_scale])) /length(which_summary_to_use)
  av_STAR <- CAM_STAR(av_CAM)


  item_stats <- matrix(item_stats, nrow = length_best_mice_scale, ncol = 8 )
  ncolssumm <- ncol(summ_fits[[1]]$ITEM_STATS)-2
  indx1 <- seq(1,ncol(item_stats), by=length_best_mice_scale)
  indx2 <- indx1 + (length_best_mice_scale-1)
  samples_mice <- out_list$CHECK$SAMPLE$n_persons - out_list$CHECK$SAMPLE$missing_values$missing_cols[match(best_mice_scale,colnames(data))]
  list_res$MUDFOLD_INFO$second_step$scale <- best_mice_scale
  list_res$MUDFOLD_INFO$second_step$excluded_items <- I[which(!I %in% best_mice_scale)]
  list_res$MUDFOLD_INFO$second_step$Lscale <- length_best_mice_scale
  list_res$MUDFOLD_INFO$second_step$CAM <- av_CAM
  list_res$MUDFOLD_INFO$second_step$CORR <- av_COR
  list_res$MUDFOLD_INFO$second_step$ADJ <- av_ADJ
  list_res$MUDFOLD_INFO$second_step$DOM <- av_DOM
  list_res$MUDFOLD_INFO$second_step$STAR <- av_STAR
  list_res$MUDFOLD_INFO$second_step$Hscale <- scale_stats[1]
  list_res$MUDFOLD_INFO$second_step$Hitem <- item_stats[,8]
  list_res$MUDFOLD_INFO$second_step$ISOitem <- item_stats[,7]
  list_res$MUDFOLD_INFO$second_step$MAXitem <- item_stats[,6]
  list_res$MUDFOLD_INFO$second_step$ISOscale <- scale_stats[2]
  list_res$MUDFOLD_INFO$second_step$MAXscale <- scale_stats[3]
  list_res$MUDFOLD_INFO$second_step$ISOitem <- item_stats[,6]
  
  list_res$MUDFOLD_INFO$second_step$OBSitem <- item_stats[,4]
  list_res$MUDFOLD_INFO$second_step$OBSscale<- scale_stats[4]
  list_res$MUDFOLD_INFO$second_step$EXPitem <- item_stats[,5]
  list_res$MUDFOLD_INFO$second_step$EXPscale<- scale_stats[5]

  
  index_inverse <- which(unlist(lapply(fits_all[which_summary_to_use], function(x){
    ests <- x$MUDFOLD_INFO$second_step$estimates$betas[best_mice_scale]
    ests[1] != 1})))
  thetas <- lapply(fits_all[which_summary_to_use], function(x) x$MUDFOLD_INFO$second_step$estimates$thetas)
  if (length(index_inverse) != 0) thetas[index_inverse] <- lapply(index_inverse, function(y) rev(thetas[[y]]))
  thetas <- Reduce("+", thetas) / length(which_summary_to_use)
  betas <- lapply(fits_all[which_summary_to_use], function(x){
    ests <- x$MUDFOLD_INFO$second_step$estimates$betas[best_mice_scale]
    if (ests[1] != 1) ests <-  abs((length_best_mice_scale+1)-ests )
    ests})
  betas <- Reduce("+", betas) / length(which_summary_to_use)
  list_res$MUDFOLD_INFO$second_step$estimates <- list(betas=betas, thetas=thetas)
  class(list_res) <- "mdf"
  return(list_res)
}



