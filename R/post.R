
make.post <- function(data) {
  post <- vector("character", length = ncol(data))
  names(post) <- colnames(data)
  post
}

check.post <- function(post, data) {
  
  if(is.null(post)) return(make.post(data))
  
  # check
  if (length(post) != ncol(data))
    stop("length(post) does not match ncol(data)", call. = FALSE)
  
  # change
  if (is.null(names(post))) names(post) <- colnames(data)
  
  post
}

