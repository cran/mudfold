
complete <- function(data, action = 1L, include = FALSE, 
                           mild = FALSE, ...) {
  if (!is.mids(data)) stop("'data' not of class 'mids'")

  m <- as.integer(data$m)
  if (is.numeric(action)) {
    action <- as.integer(action)
    idx <- action[action >= 0L & action <= m]
    if (include && all(idx != 0L)) idx <- c(0L, idx) 
    shape <- ifelse(mild, "mild", "stacked")
  }
  else if (is.character(action)) {
    if (include) idx <- 0L:m else idx <- 1L:m
    shape <- match.arg(action, c("all", "long", "broad", "repeated", "stacked"))
    shape <- ifelse(shape == "all" || mild, "mild", shape)
  }
  else stop("'action' not recognized")
  
  mylist <- vector("list", length = length(idx))
  for (j in seq_along(idx))
    mylist[[j]] <- single.complete(data$data, data$where, data$imp, idx[j])
  
  if (shape == "stacked")
    return(bind_rows(mylist))
  if (shape == "mild") {
    names(mylist) <- as.character(idx)
    class(mylist) <- c("mild", "list")
    return(mylist)
  }
  if (shape == "long") {
    cmp <- bind_rows(mylist)
    cmp <- data.frame(.imp = rep(idx, each = nrow(data$data)), 
                      .id = rep.int(1L:nrow(data$data), length(idx)), 
                      cmp)
    if (is.integer(attr(data$data, "row.names"))) 
      row.names(cmp) <- seq_len(nrow(cmp))
    else 
      row.names(cmp) <- as.character(seq_len(nrow(cmp)))
    return(cmp)
  }
  # must be broad or repeated
  cmp <- bind_cols(mylist)
  names(cmp) <- paste(rep.int(names(data$data), m), 
                           rep.int(idx, rep.int(ncol(data$data), length(idx))), 
                           sep = ".")
  if (shape == "broad") return(cmp)
  else return(cmp[, order(rep.int(seq_len(ncol(data$data)), length(idx)))])
}

single.complete <- function(data, where, imp, ell) {
  if (ell == 0L) return(data)
  if (is.null(where))
    where <- is.na(data)
  idx <- seq_len(ncol(data))[apply(where, 2, any)]
  for (j in idx) {
    if (is.null(imp[[j]])) data[where[, j], j] <- NA
    else data[where[, j], j] <- imp[[j]][, ell]
  }
  data
}
