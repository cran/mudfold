Err_exp_scale <-function(X){
  vec <- colnames(X)
  l_vec <- length(vec)
  exp <- Err_exp(X);dimnames(exp) <- list(vec,vec,vec)
  cmb1 <- combinations(n=l_vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  return(sum(exp[cmb1]))
}