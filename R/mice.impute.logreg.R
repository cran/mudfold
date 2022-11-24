
mice.impute.logreg <- function(y, ry, x, wy = NULL, ...) {
  if (is.null(wy)) wy <- !ry
  
  # augment data in order to evade perfect prediction
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w
  
  # fit model
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x = x[ry, , drop = FALSE], 
                             y = y[ry], 
                             family = quasibinomial(link = logit), 
                             weights = w[ry]))
  fit <- eval(expr)
  fit.sum <- summary.glm(fit)
  beta <- coef(fit)
  rv <- t(chol(sym(fit.sum$cov.unscaled)))
  beta.star <- beta + rv %*% rnorm(ncol(rv))
  
  # draw imputations
  p <- 1/(1 + exp(-(x[wy, , drop = FALSE] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  return(vec)
}

mice.impute.logreg.boot <- function(y, ry, x, wy =  NULL, ...) {
  if (is.null(wy)) wy <- !ry
  
  # draw a bootstrap sample for yobs and xobs
  xobs <- x[ry, , drop = FALSE]
  yobs <- y[ry]
  n1 <- sum(ry)
  s <- sample(n1, n1, replace = TRUE)
  doty <- y
  doty[ry] <- yobs[s]
  dotx <- x
  dotx[ry, ] <- xobs[s, , drop = FALSE]
  
  x <- dotx
  y <- doty
  
  # fit model
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x = x[ry, , drop = FALSE], 
                             y = y[ry], 
                             family = binomial(link = logit)))
  fit <- suppressWarnings(eval(expr))
  beta.star <- coef(fit)
  
  # draw imputations
  p <- 1/(1 + exp(-(x[wy, ] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  return(vec)
}

augment <- function(y, ry, x, wy, maxcat = 50) {
  # define augmented data for stabilizing logreg and polyreg
  # by the ad hoc procedure of White, Daniel & Royston, CSDA, 2010
  # This function will prevent augmented data beyond the min and
  # the max of the data
  # Input:
  # x: numeric data.frame (n rows)
  # y: factor or numeric vector (lengt n)
  # ry: logical vector (length n)
  # Output:
  # return a list with elements y, ry, x, and w with length n+2*(ncol(x))*length(levels(y))
  # SvB May 2009
  icod <- sort(unique(unclass(y)))
  k <- length(icod)
  if (k > maxcat) 
    stop("Maximum number of categories (", maxcat, ") exceeded")
  p <- ncol(x)
  
  # skip augmentation if there are no predictors
  if (p == 0) 
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, length(y))))
  
  ## skip augmentation if there is only 1 missing value 12jul2012 
  ## this need to be fixed 12jul2011
  if (sum(!ry) == 1) 
    return(list(y = y, ry = ry, x = x, wy = wy, w = rep(1, length(y))))
  
  # calculate values to augment
  mean <- apply(x, 2, mean, na.rm = TRUE)
  sd <- sqrt(apply(x, 2, var, na.rm = TRUE))
  minx <- apply(x, 2, min, na.rm = TRUE)
  maxx <- apply(x, 2, max, na.rm = TRUE)
  nr <- 2 * p * k
  a <- matrix(mean, nrow = nr, ncol = p, byrow = TRUE)
  b <- matrix(rep(c(rep.int(c(0.5, -0.5), k), rep.int(0, nr)), length = nr * p), nrow = nr, ncol = p, byrow = FALSE)
  c <- matrix(sd, nrow = nr, ncol = p, byrow = TRUE)
  d <- a + b * c
  d <- pmax(matrix(minx, nrow = nr, ncol = p, byrow = TRUE), d, na.rm = TRUE)
  d <- pmin(matrix(maxx, nrow = nr, ncol = p, byrow = TRUE), d, na.rm = TRUE)
  e <- rep(rep(icod, each = 2), p)
  
  dimnames(d) <- list(paste0("AUG", seq_len(nrow(d))), dimnames(x)[[2]])
  xa <- rbind.data.frame(x, d)
  # beware, concatenation of factors
  ya <- if (is.factor(y)) as.factor(levels(y)[c(y, e)]) else c(y, e)
  rya <- c(ry, rep.int(TRUE, nr))
  wya <- c(wy, rep.int(FALSE, nr))
  wa <- c(rep.int(1, length(y)), rep.int((p + 1)/nr, nr))
  
  return(list(y = ya, ry = rya, x = xa, w = wa, wy = wya))
}
