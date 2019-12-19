tidy.coef <- function(model) {
  est <- tidy(model, effects = "fixed")
  coef <- est$estimate
  names(coef) <- est$term
  coef
}