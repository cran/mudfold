data_check <- function(out){
  out_list <- out
  X <- out_list$CALL$data
  
  ########################## CHECK FUNCTION ARGUMENTS ########################
  ############################################################################
  if (is.null(out_list$CALL$method)){
    if (is.null(X) | any(dim(X)==0)) stop("Data is missing.")
    estimation <- out_list$CALL$estimation
    if (is.null(estimation)) estimation <- "rank"
    if (!estimation %in% c("rank","quantile")) stop("Provide a valid estimation method for the person parameters.\n Available options are: 'rank' and 'quantile' ")
    lambda1 <- out_list$CALL$lambda1
    if (is.null(lambda1)){
      out_list$CALL$lambda1 <- lambda1 <- 0.3
    }
    lambda2 <- out_list$CALL$lambda2
    if (is.null(lambda2)){
      out_list$CALL$lambda2 <- lambda2 <- 0
    }
  }
  
  nboot <- out_list$CALL$nboot
  if (!is.null(nboot)){
    if (round(nboot) != nboot) stop("Provide a positive integer to be used for the size of bootstrap iterations.")
    out_list$CALL$Bootstrap <- TRUE
  }else{
    out_list$CALL$Bootstrap <- FALSE
  }
  out_list$CHECK <- list()
  out_list$CHECK$warnings <- list()
  out_list$CHECK$Diagnostics <- list()
  
  ############################## CHECK DATA FORMAT #############################
  ##############################################################################
  out_list$CHECK$Diagnostics$is_matrix <- ismat <- is.matrix(X)
  out_list$CHECK$Diagnostics$is_data_frame <- isdat <- is.data.frame(X)
  out_list$CHECK$Diagnostics$is_null_names <- nullnam <- is.null(colnames(X))
  out_list$CHECK$Diagnostics$is_missing <- missdat <- any(is.na(X))
  
  ############################# CHECK NAMES ####################################
  ##############################################################################
  if (nullnam){
    colnames(X) <- paste("Item_",1:ncol(X),sep = "")
    out_list$CHECK$warnings$NULL_ITEMS <- "NO NAMES FOR THE ITEMS: Names have been generated automatically."
    warning("NO NAMES FOR THE ITEMS: Names have been generated automatically.")
  }
  
  if (!(ismat | isdat)) stop("Data should be a matrix or dataframe" )
  
  dimsX <- dim(X) # dimensions of the original data
  
  out_list$CHECK$SAMPLE <- list()
  out_list$CHECK$SAMPLE$n_persons <- dimsX[1] # number of persons
  out_list$CHECK$SAMPLE$n_items <- dimsX[2] # number of items
  
  if (dimsX[2] < 4) stop("The number of items should be larger than three.")
  
  out_list$CHECK$SAMPLE$n_persons_miss <- 0
  out_list$CHECK$SAMPLE$n_persons_compl <- dimsX[1]
  out_list$CHECK$SAMPLE$names_start <- colnames(X)
  
  
  ############################# CHECK FOR MISSINGS #############################
  ##############################################################################
  out_list$CHECK$SAMPLE$missing_values <- list()
  missarg <- out_list$CALL$missings
  out_list$CHECK$SAMPLE$missing_values$missing_rows <- apply(X,1,function(x) sum(is.na(x)))
  out_list$CHECK$SAMPLE$missing_values$missing_cols <- apply(X,2,function(x) sum(is.na(x)))
  if(missdat & missarg=="omit" ){
    miss_row <- out_list$CHECK$SAMPLE$missing_values$missing_rows >0
    out_list$CHECK$SAMPLE$missing_values$deleted_rows <- which(miss_row)
    out_list$CHECK$SAMPLE$missing_values$ndel_rows <- sum(miss_row)
    out_list$CHECK$SAMPLE$n_persons_miss <- out_list$CHECK$SAMPLE$missing_values$ndel_rows 
    out_list$CHECK$warnings$IS_MISSING <- "Rows with missing values have been removed from the data."
    warning("Rows with missing values are omitted.")
    X <- na.omit(X)
    out_list$CHECK$SAMPLE$n_persons_compl <- nrow(X)
    if (out_list$CHECK$SAMPLE$n_persons_compl == 0) stop("No data to fit after removing missings.")
    dimnames(X)[[1]] <- 1:out_list$CHECK$SAMPLE$n_persons_compl
  } 
  
  if(missdat & missarg=="impute" ){
    out_list$CHECK$SAMPLE$missing_values$deleted_rows <- which(out_list$CHECK$SAMPLE$missing_values$missing_rows >0)
    out_list$CHECK$SAMPLE$missing_values$ndel_rows <- sum(out_list$CHECK$SAMPLE$missing_values$missing_rows > 0)
    out_list$CHECK$SAMPLE$n_persons_miss <- out_list$CHECK$SAMPLE$missing_values$ndel_rows 
    out_list$CHECK$warnings$IS_MISSING <- "Rows with missing values have been imputed using 'mice' multiple imputation."
    print("missing values will be imputed using multiple imputation by chained equations from the R package mice...")
  } 
  
  
  ##################### CHECK FOR ZERO RESPONSES IN THE DATA ###################
  ##############################################################################
  out_list$CHECK$zero_responses <- list()
  zer_rows <- apply(X,1,sum, na.rm=TRUE)
  zer_cols <- apply(X,2,sum, na.rm=TRUE)
  
  if (out_list$CALL$missings != "impute"){
    if (any(zer_rows[!is.na(zer_rows)]==0)){
      out_list$CHECK$zero_responses$zero_persons <- which(zer_rows==0)
      out_list$CHECK$zero_responses$n_zero_persons <- length(out_list$CHECK$zero_responses$zero_persons)
      out_list$CHECK$warnings$NO_RESPONSE_i <- "Persons with no positive responses in the data. Consider to remove these cases."
      warning("Persons with no positive responses in the data. Consider removing these cases.")
    }
    if (any(zer_cols[!is.na(zer_cols)]==0)){
      zero_col_index <- which(zer_cols==0)
      out_list$CHECK$zero_responses$zero_items <- out_list$CHECK$SAMPLE$names_start[zero_col_index]
      out_list$CHECK$zero_responses$n_zero_items <- length(out_list$CHECK$zero_responses$zero_items)
      out_list$CHECK$warnings$NO_RESPONSE_j <- "Columns with no positive responses have been removed from the data."
      print("Columns with no positive responses removed from the data.")
      X <- X[,-zero_col_index]
    }
    
  }
  
  
  if (out_list$CALL$missings == "impute"){
    if (any(zer_rows[!is.na(zer_rows)]==0)){
      out_list$CHECK$zero_responses$zero_persons <- which(zer_rows==0)
      out_list$CHECK$zero_responses$n_zero_persons <- length(out_list$CHECK$zero_responses$zero_persons)
      out_list$CHECK$warnings$NO_RESPONSE_i <- "Persons with no positive responses in the data. Consider to remove these cases."
    }
    if (any(zer_cols[!is.na(zer_cols)]==0)){
      zero_col_index <- which(zer_cols==0)
      out_list$CHECK$zero_responses$zero_items <- out_list$CHECK$SAMPLE$names_start[zero_col_index]
      out_list$CHECK$zero_responses$n_zero_items <- length(out_list$CHECK$zero_responses$zero_items)
      out_list$CHECK$warnings$NO_RESPONSE_j <- "Columns with no positive responses have been removed from the data."
      print("Columns with no positive responses after omiting the missings have been removed from the data.")
      X <- X[,-zero_col_index]
    }
    
  }
  

  if (ncol(X) < 4) stop("The number of items in the data should be larger than or equal to four.")
  
  ####################### CALCULATE ITEM DESCRIPTIVES ####################
  ########################################################################
  Xdescr <- na.omit(X)
  out_list$DESCRIPTIVES <- list()
  out_list$DESCRIPTIVES$positives_per_item <- apply(X,2,sum,na.rm=TRUE)
  out_list$DESCRIPTIVES$positives_per_person <- apply(X,1,sum,na.rm=TRUE)
  out_list$DESCRIPTIVES$n_items_final <- ncol(X)
  out_list$DESCRIPTIVES$item_names_final <- colnames(X)
  out_list$DESCRIPTIVES$n_persons_final <- nrow(Xdescr)
  out_list$DESCRIPTIVES$prop_per_item <- out_list$DESCRIPTIVES$positives_per_item / out_list$DESCRIPTIVES$n_persons_final
  out_list$DESCRIPTIVES$prop_per_person <- out_list$DESCRIPTIVES$positives_per_person / out_list$DESCRIPTIVES$n_items_final
  
  
  out_list$MUDFOLD_INFO <- list()
  out_list$MUDFOLD_INFO$triple_stats <- list()
  out_list$MUDFOLD_INFO$triple_stats$Expected_errors <- EO <- Err_exp(na.omit(X)) # Expected errors for triples
  out_list$MUDFOLD_INFO$triple_stats$Observed_errors <- O <- Err_obs(na.omit(X)) # Observed errors for triples
  out_list$MUDFOLD_INFO$triple_stats$H_coefficients <- 1 - (O/ EO) # H coefficients for triples
  start.scale <- out_list$CALL$start.scale
  if (!is.null(start.scale) & any(!start.scale %in% colnames(X))) stop("Starting vector should consist of items in the colnames(data).")
  ############################# CHECK FOR BINARY DATA ##########################
  ##############################################################################
  if(any(!(X==0 | X==1 | is.na(X)))) stop("Data should be binary. Consider using the function pick().") 
  
  ######################### CHECK FOR SUITABLE DIMENSIONS ######################
  ##############################################################################
  if(ncol(X) > nrow(X)) stop("The number of columns should be smaller than the number of rows.")
  out_list$CALL$data_start <- out_list$CALL$data
  d <- data.frame(X)
  names(d) <- colnames(X)
  return(list(data=d,out=out_list))
}