data_check <- function(X){
  if( !is.matrix(X) & !is.data.frame(X) ) stop( "Data should be a matrix or dataframe" )
  if( any(is.na(X))){
    response <- yesno( "NA's in the data. Remove NA's?")
    if (response == 1){X <- na.omit(X)}
    else stop("Remove or impute missing values in order to continue. ")
  } 
  if (is.null(names(X))) names(X) <- 1:ncol(X)
  if( sum(!(X==0 | X==1)) > 0 ) stop("Data should be binary")
  if(ncol(X) > nrow(X) ) stop("The number of columns should be smaller than the number of rows")
  if(any(apply(X,2,sum)/nrow(X) == 0)){
    response <- yesno("Items with no responses in the data. Remove?")
    if (response== 1) X <- X[,-(which(apply(X,2,sum)/nrow(X) == 0))]
    else if (response=="N") stop("Remove items with no information from the data. ")
  }
  return(data.frame(X))
}
