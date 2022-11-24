
make.blocks <- function(data, 
                        partition = c("scatter", "collect", "void"),
                        calltype = "type") {
  if (is.vector(data) && !is.list(data)) {
    v <- as.list(as.character(data))
    names(v) <- as.character(data)
    ct <- rep(calltype, length(v))
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
    return(v)
  }
  if (is.list(data) && !is.data.frame(data)) {
    v <- name.blocks(data)
    if (length(calltype) == 1L) {
      ct <- rep(calltype, length(v))
      names(ct) <- names(v)
      attr(v, "calltype") <- ct
    }
    else {
      ct <- calltype
      names(ct) <- names(v)
      attr(v, "calltype") <- ct
    }
    return(v)
  }
  data <- as.data.frame(data)
  partition <- match.arg(partition)
  switch(partition, 
         scatter = {
           v <- as.list(names(data))
           names(v) <- names(data)
         },
         collect = {
           v <- list(names(data))
           names(v) <- "collect"
         },
         void = {
           v <- list()
         },
         {
           v <- as.list(names(data))
           names(v) <- names(data)
         })
  if (length(calltype) == 1L) {
    ct <- rep(calltype, length(v))
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
  }
  else {
    ct <- calltype
    names(ct) <- names(v)
    attr(v, "calltype") <- ct
  }
  v
}

name.blocks <- function(blocks, prefix = "B") {
  if (!is.list(blocks)) return(make.blocks(blocks))
  if (is.null(names(blocks))) names(blocks) <- rep("", length(blocks))
  inc <- 1
  for (i in seq_along(blocks)) {
    if (names(blocks)[i] != "") next
    if (length(blocks[[i]]) == 1) names(blocks)[i] <- blocks[[i]][1]
    else {
      names(blocks)[i] <- paste0(prefix, inc)
      inc <- inc + 1
    }
  }
  blocks
}

check.blocks <- function(blocks, data, calltype = "type") {
  
  data <- check.dataform(data)
  blocks <- name.blocks(blocks)
  
  # check that all variable names exists in data
  bv <- unique(unlist(blocks))
  notFound <- !bv %in% colnames(data)
  if (any(notFound)) 
    stop(paste("The following names were not found in `data`:",
               paste(bv[notFound], collapse = ", ")))
  
  if (length(calltype) == 1L) {
    ct <- rep(calltype, length(blocks))
    names(ct) <- names(blocks)
    attr(blocks, "calltype") <- ct
  }
  else {
    ct <- calltype
    names(ct) <- names(blocks)
    attr(blocks, "calltype") <- ct
  }
  
  blocks
}


construct.blocks <- function(formulas = NULL, predictorMatrix = NULL) {
  
  blocks.f <- blocks.p <- NULL
  if (!is.null(formulas)) {
    if (!all(sapply(formulas, is.formula))) return(NULL)
    blocks.f <- name.blocks(lapply(name.formulas(formulas), lhs))
    ct <- rep("formula", length(blocks.f))
    names(ct) <- names(blocks.f)
    attr(blocks.f, "calltype") <- ct
    if (is.null(predictorMatrix)) return(blocks.f)
  }
  
  if (!is.null(predictorMatrix)) {
    if (is.null(row.names(predictorMatrix)))
      stop("No row names in predictorMatrix", call. = FALSE)
    blocks.p <- name.blocks(row.names(predictorMatrix))
    ct <- rep("type", length(blocks.p))
    names(ct) <- names(blocks.p)
    attr(blocks.p, "calltype") <- ct
    if (is.null(formulas)) return(blocks.p)
  }
  
  # combine into unique blocks
  blocknames <- unique(c(names(blocks.f), names(blocks.p)))
  keep <- setdiff(blocknames, names(blocks.f))
  blocks <- c(blocks.f, blocks.p[keep])
  ct <- c(rep("formula", length(formulas)), 
          rep("type", length(keep)))
  names(ct) <- names(blocks)
  attr(blocks, "calltype") <- ct
  blocks
}

