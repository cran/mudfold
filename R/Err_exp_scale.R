Err_exp_scale <-function(X,EO){
  vec <- colnames(X)
  l_vec <- length(vec)
  cmb1 <- combinations(n=l_vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  return(sum(EO[cmb1]))
}