mudfold_fs <- function(out){
  out_list <- out
  dat <- out_list$CALL$data
  hcoeft <- out_list$MUDFOLD_INFO$triple_stats$H_coefficients
  lambda1 <- out_list$CALL$lambda1
  
  ####################################################################
  ##### FIRST STEP IN ITEM SELECTION:Find the best unique triple #####
  ####################################################################
  perm1 <- combinations(n=ncol(dat), r=3, v=colnames(dat),set = FALSE)
  perm2 <- perm1[,c(1,3,2)]
  perm3 <- perm1[,c(2,1,3)]
  
  # find unique triples
  unqtrip <- matrix(rbind(perm1[(hcoeft[perm1] > 0 & hcoeft[perm2] < 0 & hcoeft[perm3] < 0),],
                          perm2[(hcoeft[perm1] < 0 & hcoeft[perm2] > 0 & hcoeft[perm3] < 0),],
                          perm3[(hcoeft[perm1] < 0 & hcoeft[perm2] < 0 & hcoeft[perm3] > 0),]),ncol = 3)
  
  out_list$MUDFOLD_INFO$first_step <- list()
  out_list$MUDFOLD_INFO$first_step$Converged <- FALSE
  
  # Stopping criterion 1
  if (dim(unqtrip)[1] == 0){
    print("MUDFOLD DIDN'T CONVERGE: No unique triples exist.")
    return()
  } 
  out_list$MUDFOLD_INFO$first_step$unique_triples <- unqtrip
  out_list$MUDFOLD_INFO$first_step$H_unique_triples <- hcoeft[unqtrip]
  
  mHbunq <- max(out_list$MUDFOLD_INFO$first_step$H_unique_triples)
  # Stopping criterion 2
  if (mHbunq < lambda1){
    print(paste("MUDFOLD DIDN'T CONVERGE: H(unique triple) < ",lambda1,".",sep=""))
    return()
  }  
  imax <- which(out_list$MUDFOLD_INFO$first_step$H_unique_triples==mHbunq)
  
  
  ## Additional step
  if (length(imax) > 1){
    crit3 <- unqtrip[imax,]
    tcrit3 <- table(crit3)
    icrit3 <- names(tcrit3)[tcrit3 == min(tcrit3)]
    lcrit3 <- apply(crit3,1,function(x) any(icrit3 %in% x))
    if (sum(lcrit3) == 1) third_criterion <- crit3[lcrit3,]
    if (sum(lcrit3) > 1){
      crit31 <- apply(crit3[lcrit3,],1,function(x) out_list$MUDFOLD_INFO$triple_stats$Expected_errors[matrix(x,ncol = 3)])
      third_criterion <- crit3[lcrit3,][which.min(crit31),]
    }
  }else{
    third_criterion <- unqtrip[imax,]
  }
  out_list$MUDFOLD_INFO$first_step$Converged <- TRUE
  out_list$MUDFOLD_INFO$first_step$BU <- matrix(third_criterion,ncol=3)
  out_list$MUDFOLD_INFO$first_step$HBU <- mHbunq
  if (is.null(nrow(out_list$MUDFOLD_INFO$first_step$BU))){
  }else{
    out_list$MUDFOLD_INFO$first_step$maxiter_sec <- nrow(out_list$MUDFOLD_INFO$first_step$BU)
    
  }
  return(out_list)
}