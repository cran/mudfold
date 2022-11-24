diagnostics <- function(x, boot, nlambda, lambda.crit, type, k, which, plot){
  if (missing(x)) stop("argument 'x' must be provided")
  if (!inherits(x,"mdf"))  stop("x must be an object of class 'mdf'.")
  if (missing(boot)) boot <- TRUE
  if (missing(nlambda)) nlambda <- 100
  if (missing(lambda.crit)) lambda.crit <- "class"
  if (!lambda.crit %in% c("class", "deviance", "auc", "mse","mae")) stop("The 'lambda.crit' method is not correctly specified. Check the argument 'type.measure' in the cv.glmnet() function from the R package glmnet for further details. ")
  if (missing(k)) k <- 4
  if (missing(which)) which <- "all"
  if (!which %in% c("H","LI", "UM", "ISO", "MAX", "STAR","all")) stop("Available options for the argument 'which' are: 'H', 'LI', 'UM', 'ISO', 'MAX', 'STAR','all'  ")
  if (missing(plot)) plot <- TRUE
  if (missing(type)) type <- "perc"
  
  diagnost_list <- lapply(1:6, function(i) NULL)
  names(diagnost_list) <- c("H","LI", "UM", "ISO", "MAX", "STAR")
  opt1 <- x$CALL$Bootstrap == TRUE & boot == TRUE
  opt2 <- x$CALL$Bootstrap == FALSE & boot == TRUE
  opt3 <- x$CALL$Bootstrap == TRUE & boot == FALSE
  opt4 <- x$CALL$Bootstrap == FALSE & boot == FALSE
  lw <- length(which)
  if ( "H" %in% which | which == "all"){
    if (opt1){
      diagnost_list$H <- H_diagnostic(x, boot = TRUE, type = type)
    }else{
      diagnost_list$H <- H_diagnostic(x, boot = FALSE, type = type)
    }
    if (lw==1 & which == "H") return(diagnost_list$H)
  }
  if ("LI" %in% which | which == "all"){
    checkUNIQ <- apply(apply(x$CALL$data, 2, function(x) table(x)),1, function(y) any(y==1))
    if (any(checkUNIQ)){
      warning("There exist items with only one positive or negative response.\n Local independence cannot be checked in such situtations\n
                               Consider removing this item(s)")
    }else{
      new_dat <- na.omit(data.frame(x$CALL$data,theta=x$MUDFOLD_INFO$second_step$estimates$thetas))
      Ntot <- x$MUDFOLD_INFO$second_step$Lscale + 1
      items <- x$MUDFOLD_INFO$second_step$scale
      nitm <- x$MUDFOLD_INFO$second_step$Lscale 
      nrem <- x$MUDFOLD_INFO$second_step$Lscale -1
      glm_list <- lapply(1:nitm, function(i){
        remit <- items[-i]
        Y <- as.matrix(new_dat[,items[i]])
        X <- as.matrix(new_dat[, c(remit,"theta")])
        vector_pen <- rep(1, ncol(X))
        vector_pen[length(vector_pen)] <- 0
        fit_glmnet <- try(cv.glmnet(x=X, y=Y,
                                type.measure =lambda.crit, 
                                family= "binomial",
                                nlambda = nlambda,
                                #lambda = seq(0.1,0.001,length.out=nlambda),
                                intercept=FALSE,
                                standardize=FALSE,
                                maxit=1000000,
                                alpha=1,
                                penalty.factor=vector_pen))
        if (inherits(fit_glmnet,"try-error")){
          fit_glmnet <- try(cv.glmnet(x=X, y=Y,
                                      type.measure =lambda.crit, 
                                      family= "binomial",
                                      nlambda = nlambda,
                                      #lambda = seq(0.1,0.001,length.out=nlambda),
                                      intercept=FALSE,
                                      standardize=FALSE,
                                      maxit=1000000,
                                      alpha=1,
                                      penalty.factor=vector_pen))
          glm.fit <- as.matrix(coef(fit_glmnet)[-1,])
          return(glm.fit)
          
        }else{
          glm.fit <- as.matrix(coef(fit_glmnet)[-1,])
          return(glm.fit)
        }
          

        
      })
      tot_viol <- (nitm*(nitm-1))/2
      COEF_SPARSE <- matrix(NA, ncol = nitm, nrow = nitm, dimnames = list(items,items))
      for (row in 1:nitm) COEF_SPARSE[row,-row] <- unlist(glm_list[[row]][1:nrem]) 
      CI_viol <- apply(COEF_SPARSE,2, function(x) ifelse(x==0,0,1))
      diag(CI_viol) <- 0
      vec_new_mat <- matrix(NA,nitm,nitm)
      vec_new_mat <- sapply(1:nitm, function(x) sapply(1:nitm, function(y) ifelse(CI_viol[x,y]==CI_viol[y,x] & CI_viol[x,y]==1,1,0)))
      vec_new_mat <-  as.matrix(vec_new_mat)
      vec_new_mat[upper.tri(vec_new_mat,diag = TRUE)] <- 0
      dimnames(vec_new_mat) <- list(x$MUDFOLD_INFO$second_step$scale,x$MUDFOLD_INFO$second_step$scale)
      if (plot){
        image(x=1:x$MUDFOLD_INFO$second_step$Lscale,y=1:x$MUDFOLD_INFO$second_step$Lscale,z= vec_new_mat,
              xlab="", ylab=" ", xaxt='n', yaxt='n', main="Local Independence")
        axis(2, at=1:x$MUDFOLD_INFO$second_step$Lscale,labels=x$MUDFOLD_INFO$second_step$scale, col.axis="black", las=2)
        axis(1, at=1:x$MUDFOLD_INFO$second_step$Lscale,labels=x$MUDFOLD_INFO$second_step$scale, col.axis="black", las=2)
        title(ylab="Row index", line=3, cex.lab=1.2)
        title(xlab="Column index", line=4, cex.lab=1.2)
      }
      indexes_viol <- which(vec_new_mat!=0,arr.ind = T)
      diagnost_list$LI$viol <- sum(vec_new_mat)
      diagnost_list$LI$viol_stat <- diagnost_list$LI$viol / tot_viol
      diagnost_list$LI$viol_pairs <- cbind(x$MUDFOLD_INFO$second_step$scale[indexes_viol[,1]],x$MUDFOLD_INFO$second_step$scale[indexes_viol[,2]])
      if (lw==1 & which == "LI") return(diagnost_list$LI)
    } 
  }
  if ("UM" %in% which | which == "all"){
    data_new <- na.omit(data.frame(x$CALL$data[,x$MUDFOLD_INFO$second_step$scale],THETA=x$MUDFOLD_INFO$second_step$estimates$thetas))
    items <- x$MUDFOLD_INFO$second_step$scale
    Predictions <- matrix(NA, nrow = 100, ncol = x$MUDFOLD_INFO$second_step$Lscale)
    for (i in 1:x$MUDFOLD_INFO$second_step$Lscale){
      test1 <- gam(as.formula(paste(items[i],"~s(THETA, k=",k, ")",sep = "")), family = binomial, data = data_new)
      predict_test1 <- predict(test1, data.frame(THETA=seq(min(data_new$THETA),max(data_new$THETA),length.out = 100)), type = "response")
      Predictions[,i] <- predict_test1
      plot(seq(min(data_new$THETA),max(data_new$THETA),length.out = 100),
           predict_test1, 
           type = "l",
           ylim = c(0,1),
           main = paste("Estimated IRF for",items[i]),
           xlab="Theta", ylab=paste("P(",items[i],"= 1 | Theta)",sep = " " ) )
    }
    pred <- IRFs <- cbind(Predictions,seq(min(data_new$THETA),max(data_new$THETA),length.out = 100))
    dimnames(pred)[[2]] <- c(items,"THETA")
    diagnost_list$UM <- Predictions
    if (lw==1 & which == "UM") return(diagnost_list$UM)
  }
  if ("ISO" %in% which | which == "all"){
    if (opt1){
      diagnost_list$ISO <- ISO_diagnostic(x, boot = TRUE, type = type)
    }else{
      diagnost_list$ISO <- ISO_diagnostic(x, boot = FALSE, type = type)
    }
    if (lw==1 & which == "ISO") return(diagnost_list$ISO)
  }
  if ("MAX" %in% which| which == "all"){
    if (opt1){
      diagnost_list$MAX <- MAX_diagnostic(x, boot = TRUE, type = type)
    }else{
      diagnost_list$MAX <- MAX_diagnostic(x, boot = FALSE, type = type)
    }
    if (lw==1 & which == "MAX") return(diagnost_list$MAX)
  }
  
  if ("STAR" %in% which | which == "all"){
    diagnost_list$STAR <- x$MUDFOLD_INFO$second_step$STAR
    if (plot){
      star_mat <- as.matrix(x$MUDFOLD_INFO$second_step$STAR)
      star_mat <- (star_mat=="*") *1
      image(x=1:x$MUDFOLD_INFO$second_step$Lscale,y=1:x$MUDFOLD_INFO$second_step$Lscale,z= star_mat,
            xlab="", ylab=" ", xaxt='n', yaxt='n', main="Moving maxima")
      axis(2, at=1:x$MUDFOLD_INFO$second_step$Lscale,labels=x$MUDFOLD_INFO$second_step$scale, col.axis="black", las=2)
      axis(1, at=1:x$MUDFOLD_INFO$second_step$Lscale,labels=x$MUDFOLD_INFO$second_step$scale, col.axis="black", las=2)
      title(ylab="Column index", line=3, cex.lab=1.2)
      title(xlab="Row index", line=4, cex.lab=1.2)
    }
    if (lw==1 & which == "STAR") return(diagnost_list$STAR)
  }
  return(diagnost_list)
}

