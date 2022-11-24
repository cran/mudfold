mice.mids <- function(obj, maxit = 1, printFlag = TRUE, ...) {
  if (!is.mids(obj)) 
    stop("Object should be of type mids.")
  if (maxit < 1) 
    return(obj)
  
  loggedEvents <- obj$loggedEvents
  state <- list(it = 0, im = 0, co = 0, dep = "", meth = "", 
                log = !is.null(loggedEvents))
  if (is.null(loggedEvents)) 
    loggedEvents <- data.frame(it = 0, im = 0, co = 0, dep = "", 
                               meth = "", out = "")
  
  # Initialize local variables
  call <- match.call()
  imp <- obj$imp
  where <- obj$where
  if (is.null(where)) where <- is.na(obj$data)
  blocks <- obj$blocks
  if (is.null(blocks)) blocks <- make.blocks(obj$data)
  
  assign(".Random.seed", obj$lastSeedValue, pos = 1)
  
  ## OK. Iterate.
  sumIt <- obj$iteration + maxit
  from <- obj$iteration + 1
  to <- from + maxit - 1
  q <- sampler(obj$data, obj$m, where, imp, blocks, obj$method, 
               obj$visitSequence, obj$predictorMatrix, 
               obj$formulas, obj$blots, obj$post, c(from, to), printFlag, ...)
  
  imp <- q$imp
  
  ## combine with previous chainMean and chainVar
  vnames <- unique(unlist(obj$blocks))
  nvis <- length(vnames)
  if (!is.null(obj$chainMean)) {
    chainMean <- chainVar <- array(0, dim = c(nvis, to, obj$m), 
                                   dimnames = list(vnames, 
                                                   seq_len(to), paste("Chain", seq_len(obj$m))))
    for (j in seq_len(nvis)) {
      if (obj$iteration == 0) {
        chainMean[j, , ] <- q$chainMean[j, , ]
        chainVar[j, , ] <- q$chainVar[j, , ]
      } else {
        chainMean[j, seq_len(obj$iteration), ] <- obj$chainMean[j, , ]
        chainVar[j, seq_len(obj$iteration), ] <- obj$chainVar[j, , ]
        chainMean[j, from:to, ] <- q$chainMean[j, , ]
        chainVar[j, from:to, ] <- q$chainVar[j, , ]
      }
    }
  } else {
    chainMean <- chainVar <- NULL
  }
  
  if (!state$log) 
    loggedEvents <- NULL
  if (state$log) 
    row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  
  ## save, and return
  midsobj <- list(data = obj$data, imp = imp, m = obj$m,
                  where = where, blocks = obj$blocks, 
                  call = call, nmis = obj$nmis, 
                  method = obj$method,
                  predictorMatrix = obj$predictorMatrix,
                  visitSequence = obj$visitSequence,
                  formulas = obj$formulas, post = obj$post,
                  blots = obj$blots,
                  seed = obj$seed, 
                  iteration = sumIt,
                  lastSeedValue = .Random.seed, 
                  chainMean = chainMean,
                  chainVar = chainVar, 
                  loggedEvents = loggedEvents,
                  version = packageVersion("mice"),
                  date = Sys.Date())
  oldClass(midsobj) <- "mids"
  return(midsobj)
}
