
make.where <- function(data, 
                       keyword = c("missing", "all", "none", "observed")) {
  keyword <- match.arg(keyword)
  
  data <- check.dataform(data)
  where <- switch(keyword,
                  missing = is.na(data),
                  all = matrix(TRUE, nrow = nrow(data), ncol = ncol(data)),
                  none = matrix(FALSE, nrow = nrow(data), ncol = ncol(data)), 
                  observed = !is.na(data))
  
  dimnames(where) <- dimnames(data)
  where
}


check.where <- function(where, data, blocks) {
  if (is.null(where)) 
    where <- make.where(data, keyword = "missing")
  
  if (!(is.matrix(where) || is.data.frame(where)))
    if (is.character(where)) return(make.where(data, keyword = where))
  else
    stop("Argument `where` not a matrix or data frame", call. = FALSE)
  if (!all(dim(data) == dim(where)))
    stop("Arguments `data` and `where` not of same size", call. = FALSE)

  where <- as.logical(as.matrix(where))
  if (anyNA(where))
    stop("Argument `where` contains missing values", call. = FALSE)
  
  where <- matrix(where, nrow = nrow(data), ncol = ncol(data))
  dimnames(where) <- dimnames(data)
  where[, !colnames(where) %in% unlist(blocks)] <- FALSE
  where
}
