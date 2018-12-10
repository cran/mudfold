mudfold <- function(data,estimation="rank",lambda1=0.3,lambda2=0,start=NULL,nboot=NULL,...){
  call <- match.call()
  start_time <- as.POSIXct(Sys.time())
  out_list <- list()
  out_list$CALL <- list()
  out_list$CALL$data <- data
  out_list$CALL$match.call <- call
  out_list$CALL$estimation <- estimation
  out_list$CALL$lambda1 <- lambda1
  out_list$CALL$lambda2 <- lambda2
  out_list$CALL$start <- start
  out_list$CALL$nboot <- nboot
  
  ################################# CHECK THE DATA ###################################
  ####################################################################################
  check_result <- data_check(out_list)
  if (is.null(check_result)) return()
  out_list <- check_result$out
  out_list$CALL$data <- check_result$data
  
  if (!is.null(nboot)){
    if (is.null(out_list$CALL$start)){
      res_first <- mudfold_fs(out_list)
      res_second <- mudfold_se(res_first)
    }else{
      res_first <- out_list
      res_first$MUDFOLD_INFO$first_step <- NULL
      res_second <- mudfold_se(res_first)
    }
    res_second$BOOTSTRAP <- list()
    res_second$BOOTSTRAP$BOOT <- boot.mdf(data=data,nboot=nboot,start=start,estimation=estimation,lambda1=lambda1,lambda2 = lambda2,...)
    res_second$BOOTSTRAP$ORDER <- res_second$BOOTSTRAP$BOOT$t[,1]
    tab_ord <- table(res_second$BOOTSTRAP$ORDER)
    tab_bord <- names(tab_ord)[which.max(tab_ord)]
    if (is.null(tab_bord)){
      print(paste("No MUDFOLD scale obtained in ",nboot," bootstrap iterations",sep = ""))
      return()
    }
    res_second$BOOTSTRAP$BEST_ORDER <- unlist(strsplit(tab_bord,split = " "))
    res_second$BOOTSTRAP$BOOT$t0 <- as.numeric(res_second$BOOTSTRAP$BOOT$t[,-1])
    res_second$BOOTSTRAP$BOOT$t <- apply(res_second$BOOTSTRAP$BOOT$t[,-1],2,as.numeric)
    res_second$BOOTSTRAP$BOOT$t <- matrix(res_second$BOOTSTRAP$BOOT$t,
                                          ncol = 4 + 4*ncol(data),
                                          dimnames = list(1:nboot,c("H(scale)","ISO(scale)","EO(scale)","O(scale)",
                                                                    paste("H(",colnames(res_second$CALL$data),")",sep = ""),
                                                                    paste("ISO(",colnames(res_second$CALL$data),")",sep = ""), 
                                                                    paste("EO(",colnames(res_second$CALL$data),")",sep = ""),
                                                                    paste("O(",colnames(res_second$CALL$data),")",sep = ""))) )
  }else{
    if (is.null(out_list$CALL$start)){
      res_first <- mudfold_fs(out_list)
      res_second <- mudfold_se(res_first)
    }else{
      res_first <- out_list
      res_first$MUDFOLD_INFO$first_step <- NULL
      res_second <- mudfold_se(res_first)
    }
    
  }
  end_time <- as.POSIXct(Sys.time())
  res_second$CALL$elapsed_time <- difftime(end_time,start_time, units='mins')
  return(res_second)
}
