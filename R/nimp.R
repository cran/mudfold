
nimp <- function(where, blocks = make.blocks(where)) {
  #if (length(blocks) > 0 && is.null(names(blocks))) 
  #  stop("Blocks have no names. Use name.blocks(...)")
  nwhere <- apply(where, 2, sum)
  nimp <- vector("integer", length = length(blocks))
  names(nimp) <- names(blocks)
  for (i in seq_along(blocks)) nimp[i] <- sum(nwhere[blocks[[i]]])
  nimp
}
