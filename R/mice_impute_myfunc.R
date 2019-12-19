mice.impute.myfunc <-  function (y, ry, x, wy = NULL, ...) 
{
  if (is.null(wy)) 
    wy <- !ry
  aug <- augment(y, ry, x, wy)
  x <- aug$x
  y <- aug$y
  ry <- aug$ry
  wy <- aug$wy
  w <- aug$w
  x <- cbind(1, as.matrix(x))
  expr <- expression(glm.fit(x = x[ry, , drop = FALSE], y = y[ry], 
                             family = quasibinomial(link = logit), weights = w[ry], 
                             control = list(maxit=1000)))
  fit <- eval(expr)
  fit.sum <- summary.glm(fit)
  beta <- coef(fit)
  rv <- t(chol(sym(fit.sum$cov.unscaled)))
  beta.star <- beta + rv %*% rnorm(ncol(rv))
  p <- 1/(1 + exp(-(x[wy, , drop = FALSE] %*% beta.star)))
  vec <- (runif(nrow(p)) <= p)
  vec[vec] <- 1
  if (is.factor(y)) {
    vec <- factor(vec, c(0, 1), levels(y))
  }
  return(vec)
}
