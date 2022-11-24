
cbind <- function (...) {
  if (is.null(attr(list(...)[[1]], "class"))) return(base::cbind(...))
  if ("mids" %in% attr(list(...)[[1]], "class")) return(cbind.mids(...))
  else return(base::cbind(...))
}

#' @rdname cbind
#' @export
rbind <- function (...) {
  if (is.null(attr(list(...)[[1]], "class"))) return(base::rbind(...))
  if ("mids" %in% attr(list(...)[[1]], "class")) return(rbind.mids(...))
  else return(base::rbind(...))
}
