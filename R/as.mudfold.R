as.mudfold <- function(data,estimation="rank"){
  call <- match.call()
  start_time <- as.POSIXct(Sys.time())
  out_list <- list()
  out_list$CALL <- list()
  out_list$CALL$data <- data
  out_list$CALL$match.call <- call
  out_list$CALL$estimation <- estimation
  out_list$CALL$method <- "as.mudfold()"
  
  ################################# CHECK THE DATA ###################################
  ####################################################################################
  check_result <- data_check(out_list)
  if (is.null(check_result)) return()
  out_list <- check_result$out
  out_list$CALL$data <- check_result$data
  out_list$CALL$lambda1 <- "-"
  out_list$CALL$lambda2 <- "-"
  
  out_list$MUDFOLD_INFO$first_step$Converged <- "SKIPPED"
  out_list$MUDFOLD_INFO$second_step$Converged <- "SKIPPED"
  
  ########################## CALCULATE MUDFOLD STATISTICS ############################
  ####################################################################################
  
  hcoeft <- out_list$MUDFOLD_INFO$triple_stats$H_coefficients
  obserr <- out_list$MUDFOLD_INFO$triple_stats$Observed_errors
  experr <- out_list$MUDFOLD_INFO$triple_stats$Expected_errors
  estimation <- out_list$CALL$estimation
  
  b_unq <- colnames(out_list$CALL$data)
  list_res <- out_list
  list_res$MUDFOLD_INFO$second_step$scale <- b_unq
  list_res$MUDFOLD_INFO$second_step$Lscale <- length(b_unq)
  list_res$MUDFOLD_INFO$second_step$COND_ADJ <- CADJ(data[,b_unq])
  list_res$MUDFOLD_INFO$second_step$CORR <- cor(data[,b_unq])
  list_res$MUDFOLD_INFO$second_step$ADJ <- ADJ(data[,b_unq]) 
  list_res$MUDFOLD_INFO$second_step$DOM <- DOM(data[,b_unq])
  list_res$MUDFOLD_INFO$second_step$STAR <- CAM_STAR(list_res$MUDFOLD_INFO$second_step$COND_ADJ)
  list_res$MUDFOLD_INFO$second_step$Hscale <- Hscale(data[,b_unq],EO=experr,O=obserr)
  list_res$MUDFOLD_INFO$second_step$Hitem <- Hitem(data[,b_unq],EO=experr,O=obserr)
  #list_res$MUDFOLD_INFO$second_step$H_minus_item <- Hscalej(data[,b_unq],EO=experr,O=obserr)
  list_res$MUDFOLD_INFO$second_step$ISOitem <- ISO(list_res$MUDFOLD_INFO$second_step$COND_ADJ)
  list_res$MUDFOLD_INFO$second_step$ISOscale <- sum(list_res$MUDFOLD_INFO$second_step$ISOitem)
  list_res$MUDFOLD_INFO$second_step$OBSitem <- Err_obs_item(data[,b_unq],O=obserr)
  list_res$MUDFOLD_INFO$second_step$OBSscale<- Err_obs_scale(data[,b_unq],O=obserr)
  list_res$MUDFOLD_INFO$second_step$EXPitem <- Err_exp_item(data[,b_unq],EO=experr)
  list_res$MUDFOLD_INFO$second_step$EXPscale<- Err_exp_scale(data[,b_unq],EO=experr)
  if (estimation=="rank"){
    list_res$MUDFOLD_INFO$second_step$estimates <- param_est(data[,b_unq],method = "rank")
    list_res$MUDFOLD_INFO$second_step$estimates$betas <- list_res$MUDFOLD_INFO$second_step$estimates$betas[b_unq]
  }else{
    list_res$MUDFOLD_INFO$second_step$estimates <- param_est(data[,b_unq],method = "quantile")
    list_res$MUDFOLD_INFO$second_step$estimates$betas <- list_res$MUDFOLD_INFO$second_step$estimates$betas[b_unq]
  }
  class(list_res) <- "mdf"
  return(list_res)
}