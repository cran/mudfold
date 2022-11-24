
ibind <- function(x, y) {
  call <- match.call()
  call <- c(x$call, call)
  
  if (!is.mids(y) && !is.mids(x)) 
    stop("Arguments `x` and `y` not of class `mids`")
  if (!identical(is.na(x$data), is.na(y$data)))
    stop("Differences detected in the missing data pattern")
  if (!identical(x$data[!is.na(x$data)], y$data[!is.na(y$data)]))
    stop("Differences detected in the observed data")
  if (!identical(x$where, y$where)) 
    stop("Differences detected between `x$where` and `y$where`")
  if (!identical(x$blocks, y$blocks)) 
    stop("Differences detected between `x$blocks` and `y$blocks`")
  if (!identical(x$method, y$method)) 
    stop("Differences detected between `x$method` and `y$method`")
  if (!identical(x$predictorMatrix, y$predictorMatrix))
    stop("Differences detected between `x$predictorMatrix` and `y$predictorMatrix`")
  if (!identical(x$visitSequence, y$visitSequence))
    stop("Differences detected between `x$visitSequence` and `y$visitSequence`")
  if (!identical(x$post, y$post))
    stop("Differences detected between `x$post` and `y$post`")
  if (!identical(x$blots, y$blots))
    stop("Differences detected between `x$blots` and `y$blots`")
  visitSequence <- x$visitSequence
  imp <- vector("list", ncol(x$data))
  names(imp) <- names(x$data)
  for (j in visitSequence) {
    imp[[j]] <- cbind(x$imp[[j]], y$imp[[j]])
  }
  
  m <- (x$m + y$m)
  iteration <- max(x$iteration, y$iteration)
  
  chainMean <- chainVar <- initialize.chain(x$blocks, iteration, m)
  for (j in seq_len(x$m)) {
    chainMean[, seq_len(x$iteration), j] <- x$chainMean[, , j]
    chainVar[, seq_len(x$iteration), j] <- x$chainVar[, , j]
  }
  for (j in seq_len(y$m)) {
    chainMean[, seq_len(y$iteration), j + x$m] <- y$chainMean[, , j]
    chainVar[, seq_len(y$iteration), j + x$m] <- y$chainVar[, , j]
  }
  
  midsobj <- list(data = x$data, imp = imp, m = m,
                  where = x$where, blocks = x$blocks, 
                  call = call, nmis = x$nmis, 
                  method = x$method,
                  predictorMatrix = x$predictorMatrix,
                  visitSequence = visitSequence, 
                  formulas = x$formulas, post = x$post,
                  blots = x$blots,
                  seed = x$seed, 
                  iteration = iteration,
                  lastSeedValue = .Random.seed, 
                  chainMean = chainMean,
                  chainVar = chainVar, 
                  loggedEvents = x$loggedEvents, 
                  version = packageVersion("mice"),
                  date = Sys.Date())
  oldClass(midsobj) <- "mids"
  return(midsobj)
}
