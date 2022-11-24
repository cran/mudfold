
quickpred <- function(data, mincor = 0.1, minpuc = 0, include = "", exclude = "", method = "pearson") {
  # automatic predictor selection according to Van Buuren et al (1999)
  
  # argument checking
  data <- check.dataform(data)
  
  # initialize
  nvar <- ncol(data)
  predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(names(data), names(data)))
  x <- data.matrix(data)
  r <- !is.na(x)
  
  # include predictors with 1) pairwise correlation among data 2) pairwise correlation of data with response indicator
  # higher than mincor
  suppressWarnings(v <- abs(cor(x, use = "pairwise.complete.obs", method = method)))
  v[is.na(v)] <- 0
  suppressWarnings(u <- abs(cor(y = x, x = r, use = "pairwise.complete.obs", method = method)))
  u[is.na(u)] <- 0
  maxc <- pmax(v, u)
  predictorMatrix[maxc > mincor] <- 1
  
  # exclude predictors with a percentage usable cases below minpuc
  p <- md.pairs(data)
  puc <- p$mr/(p$mr + p$mm)
  predictorMatrix[puc < minpuc] <- 0
  
  # exclude predictors listed in the exclude argument
  yz <- pmatch(exclude, names(data))
  predictorMatrix[, yz] <- 0
  
  # include predictors listed in the include argument
  yz <- pmatch(include, names(data))
  predictorMatrix[, yz] <- 1
  
  # some final processing
  diag(predictorMatrix) <- 0
  predictorMatrix[colSums(!r) == 0, ] <- 0
  
  return(predictorMatrix)
}
