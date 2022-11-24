make.blots <- function(data, blocks = make.blocks(data)) {
  data <- check.dataform(data)
  blots <- vector("list", length(blocks))
  for (i in seq_along(blots)) blots[[i]] <- alist()
  names(blots) <- names(blocks)
  blots
}

check.blots <- function(blots, data, blocks = NULL) {
  data <- check.dataform(data)

  if (is.null(blots)) return(make.blots(data, blocks))
  
  blots <- as.list(blots)
  for (i in seq_along(blots)) blots[[i]] <- as.list(blots[[i]])
  
  if (length(blots) == length(blocks) && is.null(names(blots)))
    names(blots) <- names(blocks)
  blots
}

