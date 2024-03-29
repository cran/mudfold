
make.predictorMatrix <- function(data, blocks = make.blocks(data)) {
  data <- check.dataform(data)
  predictorMatrix <- matrix(1, nrow = length(blocks), ncol = ncol(data))
  dimnames(predictorMatrix) <- list(names(blocks), colnames(data))
  for (i in row.names(predictorMatrix)) 
    predictorMatrix[i, colnames(predictorMatrix) %in% i] <- 0
  predictorMatrix
}

check.predictorMatrix <- function(predictorMatrix, 
                                  data,
                                  blocks = NULL) {
  data <- check.dataform(data)

  if (!is.matrix(predictorMatrix))
    stop("predictorMatrix not a matrix", call. = FALSE)
  if (any(dim(predictorMatrix) == 0L))
    stop("predictorMatrix has no rows or columns", call. = FALSE)
  
  # if we have no blocks, restrict to square predictorMatrix
  if (is.null(blocks)) {
    if (nrow(predictorMatrix) != ncol(predictorMatrix))
      stop(paste("If no blocks are specified, predictorMatrix must", 
                 "have same number of rows and columns"), 
           call. = FALSE)
    if (is.null(dimnames(predictorMatrix))) {
      if (ncol(predictorMatrix) == ncol(data)) 
        dimnames(predictorMatrix) <- list(colnames(data), colnames(data))
      else
        stop("Missing row/column names in predictorMatrix", call. = FALSE)
    }
    for (i in row.names(predictorMatrix))
      predictorMatrix[i, grep(i, colnames(predictorMatrix), fixed = TRUE)] <- 0
    return(predictorMatrix)
  }
  
  # check conforming arguments
  if (nrow(predictorMatrix) > length(blocks))
    stop(paste0("predictorMatrix has more rows (", nrow(predictorMatrix), 
                ") than blocks (", length(blocks), ")"),
         call. = FALSE)
  
  # borrow rownames from blocks if needed
  if (is.null(rownames(predictorMatrix)) && 
      nrow(predictorMatrix) == length(blocks))
    rownames(predictorMatrix) <- names(blocks)
  if (is.null(rownames(predictorMatrix)))
    stop("Unable to set row names of predictorMatrix", call. = FALSE)
  
  # borrow blocknames from predictorMatrix if needed
  if (is.null(names(blocks)) &&
      nrow(predictorMatrix) == length(blocks))
    names(blocks) <- rownames(predictorMatrix)
  if (is.null(names(blocks)))
    stop("Unable to set names of blocks", call. = FALSE)
  
  # check existence of row names in blocks
  found <- rownames(predictorMatrix) %in% names(blocks)
  if (!all(found))
    stop("Names not found in blocks: ", 
         paste(rownames(predictorMatrix)[!found], collapse = ", "), 
         call. = FALSE)
  
  # borrow colnames from data if needed
  if (is.null(colnames(predictorMatrix)) && 
      ncol(predictorMatrix) == ncol(data)) 
    colnames(predictorMatrix) <- names(data)
  if (is.null(colnames(predictorMatrix))) 
    stop("Unable to set column names of predictorMatrix", call. = FALSE)
  
  # check existence of variable names on data
  found <- colnames(predictorMatrix) %in% names(data) 
  if (!all(found))
    stop("Names not found in data: ", 
         paste(colnames(predictorMatrix)[!found], collapse = ", "), 
         call. = FALSE)
  
  list(predictorMatrix = predictorMatrix,
       blocks = blocks)
}
