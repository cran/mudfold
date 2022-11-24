
mice.impute.sample <- function(y, ry, x = NULL, wy = NULL, ...)
{
  if (is.null(wy)) 
    wy <- !ry
  yry <- y[ry]
  if (length(yry) < 1) 
    return(rnorm(sum(wy)))
  if (length(yry) == 1) yry <- rep(yry, 2)
  return(sample(yry, size = sum(wy), replace = TRUE))
}
