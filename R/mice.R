
mice <- function(data, m = 5, 
                 method = NULL,
                 predictorMatrix,
                 where = NULL,
                 blocks,
                 visitSequence = NULL,
                 formulas,
                 blots = NULL,
                 post = NULL,
                 defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
                 maxit = 5, printFlag = TRUE, seed = NA,
                 data.init = NULL,
                 ...) {
  call <- match.call()
  check.deprecated(...)
  if (!is.na(seed)) set.seed(seed)
  
  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)
  
  # determine input combination: predictorMatrix, blocks, formulas
  mp <- missing(predictorMatrix)
  mb <- missing(blocks)
  mf <- missing(formulas)
  
  # case A
  if (mp & mb & mf) {
    # blocks lead
    blocks <- make.blocks(colnames(data))
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
  }
  # case B
  if (!mp & mb & mf) {
    # predictorMatrix leads
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- make.blocks(colnames(predictorMatrix), partition = "scatter")
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
  }
  
  # case C
  if (mp & !mb & mf) {
    # blocks leads
    blocks <- check.blocks(blocks, data)
    predictorMatrix <- make.predictorMatrix(data, blocks)
    formulas <- make.formulas(data, blocks)
  }
  
  # case D
  if (mp & mb & !mf) {
    # formulas leads
    formulas <- check.formulas(formulas, data)
    blocks <- construct.blocks(formulas)
    predictorMatrix <- make.predictorMatrix(data, blocks)
  }
  
  # case E
  if (!mp & !mb & mf) {
    # predictor leads
    blocks <- check.blocks(blocks, data)
    z <- check.predictorMatrix(predictorMatrix, data, blocks)
    predictorMatrix <- z$predictorMatrix
    blocks <- z$blocks
    formulas <- make.formulas(data, blocks, predictorMatrix = predictorMatrix)
  }
  
  # case F
  if (!mp & mb & !mf) {
    # formulas lead
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data)
    blocks <- construct.blocks(formulas, predictorMatrix)
  }
  
  # case G
  if (mp & !mb & !mf) {
    # blocks lead
    blocks <- check.blocks(blocks, data, calltype = "formula")
    formulas <- check.formulas(formulas, blocks)
    predictorMatrix <- make.predictorMatrix(data, blocks)
  }
  
  # case H
  if (!mp & !mb & !mf) {
    # blocks lead
    blocks <- check.blocks(blocks, data)
    formulas <- check.formulas(formulas, data)
    predictorMatrix <- check.predictorMatrix(predictorMatrix, data, blocks)
  }
  
  chk <- check.cluster(data, predictorMatrix)  
  where <- check.where(where, data, blocks)
  visitSequence <- check.visitSequence(visitSequence, data = data, 
                                       where = where, blocks = blocks)
  method <- check.method(method = method, data = data, where = where, 
                         blocks = blocks, defaultMethod = defaultMethod)
  post <- check.post(post, data)
  blots <- check.blots(blots, data, blocks)
  
  # data frame for storing the event log
  state <- list(it = 0, im = 0, dep = "", meth = "", log = FALSE)
  loggedEvents <- data.frame(it = 0, im = 0, dep = "", meth = "", out = "")
  
  # edit imputation setup
  setup <- list(method = method,
                predictorMatrix = predictorMatrix,
                visitSequence = visitSequence, 
                post = post)
  setup <- edit.setup(data, setup, ...)
  method <- setup$method
  predictorMatrix <- setup$predictorMatrix
  visitSequence <- setup$visitSequence
  post <- setup$post

  # initialize imputations
  nmis <- apply(is.na(data), 2, sum)
  imp <- initialize.imp(data, m, where, blocks, visitSequence, 
                        method, nmis, data.init)
  
  # and iterate...
  from <- 1
  to <- from + maxit - 1
  q <- sampler(data, m, where, imp, blocks, method, visitSequence, 
               predictorMatrix, formulas, blots, post, c(from, to), 
               printFlag, ...)
  
  if (!state$log) loggedEvents <- NULL
  if (state$log) row.names(loggedEvents) <- seq_len(nrow(loggedEvents))
  
  ## save, and return
  midsobj <- list(data = data, imp = q$imp, m = m,
                  where = where, blocks = blocks, 
                  call = call, nmis = nmis, 
                  method = method,
                  predictorMatrix = predictorMatrix,
                  visitSequence = visitSequence,
                  formulas = formulas, post = post, 
                  blots = blots,
                  seed = seed, 
                  iteration = q$iteration,
                  lastSeedValue = .Random.seed, 
                  chainMean = q$chainMean,
                  chainVar = q$chainVar, 
                  loggedEvents = loggedEvents,
                  version = packageVersion("mice"),
                  date = Sys.Date())
  oldClass(midsobj) <- "mids"
  
  if (!is.null(midsobj$loggedEvents)) 
    warning("Number of logged events: ", nrow(midsobj$loggedEvents),
            call. = FALSE)
  return(midsobj)
}
