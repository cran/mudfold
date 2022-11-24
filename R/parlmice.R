
parlmice <- function(data, m = 5, seed = NA, cluster.seed = NA, n.core = NULL, 
                     n.imp.core = NULL, cl.type = "PSOCK", ...){ 
  # check form of data and m
  data <- check.dataform(data)
  m <- check.m(m)
  
  # check if data complete
  if (sum(is.na(data)) == 0){
    stop("Data has no missing values")
  }
  
  # check if arguments match CPU specifications
  if (!is.null(n.core)){
    if(n.core > parallel::detectCores()){ 
      stop("Number of cores specified is greater than the number of logical cores in your CPU")
    }
  } 
  
  # determine course of action when not all arguments specified
  if (!is.null(n.core) & is.null(n.imp.core)){
    n.imp.core = m
    warning(paste("Number of imputations per core not specified: n.imp.core = m =", m, "has been used"))
  }
  if (is.null(n.core) & !is.null(n.imp.core)){
    n.core = parallel::detectCores() - 1
    warning(paste("Number of cores not specified. Based on your machine a value of n.core =", parallel::detectCores()-1, "is chosen"))
  }
  if (is.null(n.core) & is.null(n.imp.core)) {
    specs <- match.cluster(n.core = parallel::detectCores() - 1, m = m)
    n.core = specs$cores
    n.imp.core = specs$imps
  }
  if (!is.na(seed)){
    if(n.core > 1){
      warning("Be careful; the specified seed is equal for all imputations. Please consider specifying cluster.seed instead.")
    }
  } 
  
  # create arguments to export to cluster
  args <- match.call(mice, expand.dots = TRUE)
  args[[1]] <- NULL
  args$m <- n.imp.core

  # make computing cluster
  cl <- parallel::makeCluster(n.core, type = cl.type)
  parallel::clusterExport(cl, 
                          varlist = c("data", "m", "seed", "cluster.seed", 
                                      "n.core", "n.imp.core", "cl.type",
                                      ls(parent.frame())), 
                          envir = environment())
  parallel::clusterExport(cl, 
                          varlist = "do.call")
  parallel::clusterEvalQ(cl, library(mice))
  if (!is.na(cluster.seed)) {
    parallel::clusterSetRNGStream(cl, cluster.seed)
  }

  # generate imputations
  imps <- parallel::parLapply(cl = cl, X = 1:n.core, function(x) do.call(mice, as.list(args), envir = environment()))
  parallel::stopCluster(cl)
  
  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  for(i in 1:length(imp$imp)){ #let imputation matrix correspond to grand m
    colnames(imp$imp[[i]]) <- 1:imp$m
  }
  
  return(imp)
}

match.cluster <- function(n.core, m){
  cores <- 1:n.core
  imps <- 1:m
  out <- data.frame(results = as.vector(cores %*% t(imps)),
                    cores = cores,
                    imps = rep(imps, each = n.core))
  which  <- out[out[, "results"] == m, ]
  which[order(which$cores, decreasing = T), ][1, 2:3]
}
