
fix.coef <- function(model, beta = NULL) {
  oldcoef <- tidy.coef(model)
  if (is.null(beta)) beta <- oldcoef
  if (length(oldcoef) != length(beta)) 
    stop("incorrect length of 'beta'", call. = FALSE)
  
  # handle naming
  if (is.null(names(oldcoef)))
    names(oldcoef) <- make.names(seq_along(oldcoef))
  if (is.null(names(beta))) names(beta) <- names(oldcoef)
  else {
    diff <- setdiff(names(oldcoef), names(beta))
    if (length(diff) > 0)
      stop("names not found in 'beta': ", diff, call. = FALSE)
    diff <- setdiff(names(beta), names(oldcoef))
    if (length(diff) > 0)
      stop("names not found in 'coef(model)': ", diff, call. = FALSE)
  }
  beta <- beta[names(oldcoef)]
  
  # re-calculate model for new beta's
  data <- model.frame(formula = formula(model), data = model.frame(model))
  mm <- model.matrix(formula(model, fixed.only = TRUE), data = data)
  offset <- as.vector(mm %*% beta)
  uf <- . ~ 1
  if (inherits(model, "merMod")) uf <- formula(model, random.only = TRUE)
  upd <- update(model, formula. = uf, 
         data = cbind(data, offset = offset),
         offset = offset)
  upd
}

tidy.coef <- function(model) {
  est <- tidy(model, effects = "fixed")
  coef <- est$estimate
  names(coef) <- est$term
  coef
}
