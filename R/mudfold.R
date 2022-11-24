
mudfold <- function(data, estimation, lambda1, lambda2, start.scale, nboot, missings, nmice, seed, mincor, ...){
  call <- match.call()
  start_time <- as.POSIXct(Sys.time())
  if (missing(lambda1)) lambda1 <- 0.3
  if (missing(lambda2)) lambda2 <- 0
  if (missing(seed)) seed <- NULL
  if (missing(start.scale)) start.scale <- NULL
  if (missing(nboot)) nboot <- NULL
  if (missing(estimation)) estimation <- "rank"
  if (missing(missings)) missings <- "omit"
  if (missing(mincor)) mincor <- 0.1
  if (missing(nmice) & missings =="omit") nmice <- NULL
  if (missing(nmice) & missings=="impute") nmice <- 10 
  out_list <- list()
  out_list$CALL <- list()
  out_list$CALL$data <- data
  out_list$CALL$match.call <- call
  out_list$CALL$estimation <- estimation
  out_list$CALL$mincor <- mincor

  out_list$CALL$lambda1 <- lambda1
  out_list$CALL$lambda2 <- lambda2
  out_list$CALL$start.scale <- start.scale
  out_list$CALL$nboot <- nboot
  out_list$CALL$seed <- seed
  out_list$CALL$missings <- missings
  out_list$CALL$nmice <- nmice

  
  if (any(is.na(data)) & missings == "impute"){
    check_result <- data_check(out_list)
    if (is.null(check_result)) return()
    out_list <- check_result$out
    out_list$CALL$data <- check_result$data
    if (is.null(nboot)){
      res_second <- mice_mudfold_noboot(out_list)
    }else{
      list_res <- list()
      list_res <- out_list
      list_res$BOOTSTRAP <- list()
      if (is.null(seed)){
        list_res$BOOTSTRAP$BOOT <- boot.mdf(data= out_list$CALL$data,nboot=nboot,start=start.scale,estimation=estimation,lambda1=lambda1,lambda2 = lambda2,out=out_list, ...)
      }else{
        list_res$BOOTSTRAP$BOOT <- boot.mdf(data=out_list$CALL$data,nboot=nboot,start=start.scale,estimation=estimation,lambda1=lambda1,lambda2 = lambda2, seed = seed, out=out_list, ...)
      }
      list_res$BOOTSTRAP$ORDER <- list_res$BOOTSTRAP$BOOT$t[,1]
      for(i in 1:length(list_res$BOOTSTRAP$ORDER)){
        to_seek <- list_res$BOOTSTRAP$ORDER[i]
        which_equal <- which(list_res$BOOTSTRAP$ORDER == to_seek)
        indexscale <- paste(rev(unlist(strsplit(to_seek,split = " "))), collapse = " ")
        list_res$BOOTSTRAP$ORDER[which(list_res$BOOTSTRAP$ORDER == indexscale)] <- to_seek
      }
      tab_ord <- table(list_res$BOOTSTRAP$ORDER)
      tab_bord <- names(tab_ord)[which.max(tab_ord)]
      if (is.null(tab_bord)){
        print(paste("No MUDFOLD scale obtained in ",nboot," bootstrap iterations",sep = ""))
        return()
      }
      list_res$BOOTSTRAP$BEST_ORDER <- unlist(strsplit(tab_bord,split = " "))
      b_unq <- list_res$BOOTSTRAP$BEST_ORDER
      dt_to_use <- na.omit(out_list$CALL$data)

      list_res$MUDFOLD_INFO$second_step <-list()
      list_res$MUDFOLD_INFO$first_step <-list()
      list_res$MUDFOLD_INFO$first_step$Converged <- FALSE
      list_res$MUDFOLD_INFO$second_step$Converged <- TRUE
      experr <- out_list$MUDFOLD_INFO$triple_stats$Expected_errors
      obserr <- out_list$MUDFOLD_INFO$triple_stats$Observed_errors
      list_res$MUDFOLD_INFO$second_step$scale <- b_unq
      
      list_res$MUDFOLD_INFO$second_step$excluded_items <- out_list$DESCRIPTIVES$item_names_final[which(!out_list$DESCRIPTIVES$item_names_final %in% list_res$BOOTSTRAP$BEST_ORDER)]
      list_res$MUDFOLD_INFO$second_step$Lscale <- length(b_unq)
      list_res$MUDFOLD_INFO$second_step$CAM <- CAM(dt_to_use[,b_unq])
      list_res$MUDFOLD_INFO$second_step$CORR <- cor(dt_to_use[,b_unq])
      list_res$MUDFOLD_INFO$second_step$ADJ <- ADJ(dt_to_use[,b_unq]) 
      list_res$MUDFOLD_INFO$second_step$DOM <- DOM(dt_to_use[,b_unq])
      list_res$MUDFOLD_INFO$second_step$STAR <- CAM_STAR(list_res$MUDFOLD_INFO$second_step$CAM)
      list_res$MUDFOLD_INFO$second_step$Hscale <- Hscale(dt_to_use[,b_unq],EO=experr,O=obserr)
      list_res$MUDFOLD_INFO$second_step$Hitem <- Hitem(dt_to_use[,b_unq],EO=experr,O=obserr)
      #list_res$MUDFOLD_INFO$second_step$H_minus_item <- Hscalej(data[,b_unq],EO=experr,O=obserr)
      list_res$MUDFOLD_INFO$second_step$ISOitem <- ISO(list_res$MUDFOLD_INFO$second_step$CAM)
      list_res$MUDFOLD_INFO$second_step$ISOscale <- sum(list_res$MUDFOLD_INFO$second_step$ISOitem)
      list_res$MUDFOLD_INFO$second_step$MAXitem <- MAX(list_res$MUDFOLD_INFO$second_step$CAM)
      list_res$MUDFOLD_INFO$second_step$MAXscale <- sum(list_res$MUDFOLD_INFO$second_step$MAXitem) 
      list_res$MUDFOLD_INFO$second_step$OBSitem <- Err_obs_item(dt_to_use[,b_unq],O=obserr)
      list_res$MUDFOLD_INFO$second_step$OBSscale<- Err_obs_scale(dt_to_use[,b_unq],O=obserr)
      list_res$MUDFOLD_INFO$second_step$EXPitem <- Err_exp_item(dt_to_use[,b_unq],EO=experr)
      list_res$MUDFOLD_INFO$second_step$EXPscale<- Err_exp_scale(dt_to_use[,b_unq],EO=experr)
      if (estimation=="rank"){
        list_res$MUDFOLD_INFO$second_step$estimates <- param_est(out_list$CALL$data[,b_unq],method = "rank")
        list_res$MUDFOLD_INFO$second_step$estimates$betas <- list_res$MUDFOLD_INFO$second_step$estimates$betas[b_unq]
      }else{
        list_res$MUDFOLD_INFO$second_step$estimates <- param_est(out_list$CALL$data[,b_unq],method = "quantile")
        list_res$MUDFOLD_INFO$second_step$estimates$betas <- list_res$MUDFOLD_INFO$second_step$estimates$betas[b_unq]
      }
      
      list_res$BOOTSTRAP$BOOT$t0 <- as.numeric(list_res$BOOTSTRAP$BOOT$t0[,-1])
      list_res$BOOTSTRAP$BOOT$t <- apply(list_res$BOOTSTRAP$BOOT$t[,-1],2,as.numeric)
      list_res$BOOTSTRAP$BOOT$t <- matrix(list_res$BOOTSTRAP$BOOT$t,
                                            ncol = 5 + 5*ncol(data),
                                            dimnames = list(1:nboot,c("H(scale)","ISO(scale)","MAX(scale)", "EO(scale)","O(scale)",
                                                                      paste("H(",colnames(list_res$CALL$data),")",sep = ""),
                                                                      paste("ISO(",colnames(list_res$CALL$data),")",sep = ""), 
                                                                      paste("MAX(",colnames(list_res$CALL$data),")",sep = ""), 
                                                                      paste("EO(",colnames(list_res$CALL$data),")",sep = ""),
                                                                      paste("O(",colnames(list_res$CALL$data),")",sep = ""))) )
      class(list_res) <- "mdf"
      res_second <- list_res
    }
  }
  if ((any(is.na(data)) & missings == "omit") | (!any(is.na(data)))){
    check_result <- data_check(out_list)
    if (is.null(check_result)) return()
    out_list <- check_result$out
    out_list$CALL$data <- check_result$data
    if (!is.null(nboot)){
      if (is.null(out_list$CALL$start.scale)){
        res_first <- mudfold_fs(out_list)
        res_second <- mudfold_se(res_first)
      }else{
        res_first <- out_list
        res_first$MUDFOLD_INFO$first_step <- NULL
        res_second <- mudfold_se(res_first)
      }
      res_second$BOOTSTRAP <- list()
      if (is.null(seed)){
        res_second$BOOTSTRAP$BOOT <- boot.mdf(data=out_list$CALL$data,nboot=nboot,start=start.scale,estimation=estimation,lambda1=lambda1,lambda2 = lambda2,out=out_list,...)
      }else{
        res_second$BOOTSTRAP$BOOT <- boot.mdf(data=out_list$CALL$data,nboot=nboot,start=start.scale,estimation=estimation,lambda1=lambda1,lambda2 = lambda2,seed = seed,out=out_list, ...)
      }
      res_second$BOOTSTRAP$ORDER <- res_second$BOOTSTRAP$BOOT$t[,1]
      for(i in 1:length(res_second$BOOTSTRAP$ORDER)){
        to_seek <- res_second$BOOTSTRAP$ORDER[i]
        which_equal <- which(res_second$BOOTSTRAP$ORDER == to_seek)
        indexscale <- paste(rev(unlist(strsplit(to_seek,split = " "))), collapse = " ")
        res_second$BOOTSTRAP$ORDER[which(res_second$BOOTSTRAP$ORDER == indexscale)] <- to_seek
      }
      tab_ord <- table(res_second$BOOTSTRAP$ORDER)
      tab_bord <- names(tab_ord)[which.max(tab_ord)]
      if (is.null(tab_bord)){
        print(paste("No MUDFOLD scale obtained in ",nboot," bootstrap iterations",sep = ""))
        return()
      }
      res_second$BOOTSTRAP$BEST_ORDER <- unlist(strsplit(tab_bord,split = " "))
      res_second$BOOTSTRAP$BOOT$t0 <- as.numeric(res_second$BOOTSTRAP$BOOT$t0[,-1])
      res_second$BOOTSTRAP$BOOT$t <- apply(res_second$BOOTSTRAP$BOOT$t[,-1],2,as.numeric)
      res_second$BOOTSTRAP$BOOT$t <- matrix(res_second$BOOTSTRAP$BOOT$t,
                                            ncol = 5 + 5*ncol(out_list$CALL$data),
                                            dimnames = list(1:nboot,c("H(scale)","ISO(scale)","MAX(scale)","EO(scale)","O(scale)",
                                                                      paste("H(",colnames(res_second$CALL$data),")",sep = ""),
                                                                      paste("ISO(",colnames(res_second$CALL$data),")",sep = ""), 
                                                                      paste("MAX(",colnames(res_second$CALL$data),")",sep = ""), 
                                                                      paste("EO(",colnames(res_second$CALL$data),")",sep = ""),
                                                                      paste("O(",colnames(res_second$CALL$data),")",sep = ""))) )
    }else{
      if (is.null(out_list$CALL$start.scale)){
        res_first <- mudfold_fs(out_list)
        res_second <- mudfold_se(res_first)
      }else{
        res_first <- out_list
        res_first$MUDFOLD_INFO$first_step <- NULL
        res_second <- mudfold_se(res_first)
      }
    }
  }
  end_time <- as.POSIXct(Sys.time())
  res_second$CALL$elapsed_time <- difftime(end_time,start_time, units='mins')
  return(res_second)
}
    
    
  
  