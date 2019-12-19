print.mdf <- function(x, ...){
  cat("\nCall: ")
  print(x$CALL$match.call)
  cat("\n")
  cat("Time elapsed:",x$CALL$elapsed_time," minutes \n")
  if (length(x$CHECK$warnings)>0) invisible(sapply(unlist(unname(x$CHECK$warnings)),function(x) cat(paste("\nWarning:",x,sep = " "))))
  cat("\n")
  if (!is.null(x$CHECK$SAMPLE$missing_values$ndel_rows)){
    cat("\nMissing values:","TRUE" )
    cat("\nNumber of cases with missings:",x$CHECK$SAMPLE$missing_values$ndel_rows)
    cat("\nTotal number of missings:",sum(x$CHECK$SAMPLE$missing_values$missing_rows))
    cat("\nMissings per item:",x$CHECK$SAMPLE$missing_values$missing_cols)
    mat_miss <- matrix(NA, ncol = length(x$CHECK$SAMPLE$missing_values$deleted_rows),nrow = 1,dimnames = list("nmiss",x$CHECK$SAMPLE$missing_values$deleted_rows) )
    mat_miss[1,] <- x$CHECK$SAMPLE$missing_values$missing_rows[x$CHECK$SAMPLE$missing_values$deleted_rows]
    cat("\nPersons with missings:")
    print(mat_miss)
    if (x$CALL$missings=="impute"){
      if (!x$CALL$Bootstrap){
        cat("\nMultiple imputation (mice):","YES")
        cat("\nTotal number of mice imputations:",x$CALL$nmice,"\n")
      }else{
        cat("\nMultiple imputation (mice):","YES")
        cat("\nTotal number of mice imputations:",x$CALL$nboot,"\n")
      }
    }
  } 
  if (!is.null(x$CHECK$zero_responses$n_zero_persons)){
    if (is.null(x$CHECK$SAMPLE$missing_values$ndel_rows)){
      cat("\nZero responses:","TRUE")
      cat("\nNumber of persons with zero responses:",x$CHECK$zero_responses$n_zero_persons)
      cat("\nNumber of items with zero responses:",x$CHECK$zero_responses$n_zero_items,"\n")
    }else{
      cat("\nZero responses (when missings are ignored):","TRUE")
      cat("\nNumber of persons with zero responses:",x$CHECK$zero_responses$n_zero_persons)
      cat("\nNumber of items with zero responses:",x$CHECK$zero_responses$n_zero_items,"\n")
    }
  
  } 

  cat("\nIndividuals:", x$DESCRIPTIVES$n_persons_final)
  cat("\nItems:", x$DESCRIPTIVES$n_items_final,"\n")
  cat("\nEstimation method:",x$CALL$estimation)
  if (!is.null(x$CALL$start)) cat("\nStarting scale:",x$CALL$start)
  cat("\nLambda1:",x$CALL$lambda1)
  cat("\nLambda2:",x$CALL$lambda2,"\n")
  
  if (x$CALL$Bootstrap){
    x$BOOTSTRAP$BOOT$t <- apply(x$BOOTSTRAP$BOOT$t,2,function(x){
      vi <- x;v <- na.omit(x);lv <- length(v);vv <- unique(v);lvv <- length(vv)
      if (lvv==1){
        val <- ifelse(vv==1,0.99999,0.0000011)
        ind <- sample(1:lv,max(1,round(lv/30)));v[ind] <- val
        vi[!is.na(vi)] <-v
      }
      return(vi)
    })
    cat("Bootstrap:",ifelse(x$CALL$Bootstrap,"YES","NO"),"\n")
    cat("Bootstrap replications:",x$CALL$nboot,"\n")
  }
  cat("\nFirst step completed:",x$MUDFOLD_INFO$first_step$Converged)
  cat("\nSecond step completed: ",x$MUDFOLD_INFO$second_step$Converged,"\n")
  if (x$MUDFOLD_INFO$second_step$Converged){
    cat("\nscalability H for the MUDFOLD scale:",x$MUDFOLD_INFO$second_step$Hscale)
    if (x$CALL$Bootstrap){
      cat("\nBootstrap 95% percentile CI for the H coef:",
          paste("(",paste(round(boot.ci(x$BOOTSTRAP$BOOT,type = "perc",index = 1)$percent[,4:5],3),collapse = ", "),")",sep = ""))
    }
    cat("\nIso statistic for the MUDFOLD scale:",x$MUDFOLD_INFO$second_step$ISOscale)
    if (x$CALL$Bootstrap){
      cat("\nBootstrap 95% percentile CI for the ISO statistic:",
          paste("(",paste(round(boot.ci(x$BOOTSTRAP$BOOT,type = "perc",index = 2)$percent[,4:5],3),collapse = ", "),")",sep = ""),"\n")
    }
    cat("\nMax statistic for the MUDFOLD scale:",x$MUDFOLD_INFO$second_step$MAXscale)
    if (x$CALL$Bootstrap){
      cat("\nBootstrap 95% percentile CI for the MAX statistic:",
          paste("(",paste(round(boot.ci(x$BOOTSTRAP$BOOT,type = "perc",index = 3)$percent[,4:5],3),collapse = ", "),")",sep = ""),"\n")
      cat("\nSummary of bootstrap iterations:\n")
      print(summary(x$BOOTSTRAP$BOOT$t)[,1:3])
    }
    
    cat("\n")
  }else{
    cat("\n")
  }
}