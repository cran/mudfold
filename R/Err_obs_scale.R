Err_obs_scale <-function(X){
  vec <- colnames(X)
  l_vec <- length(vec)
  o <-Err_obs(X);dimnames(o) <- list(vec, vec, vec)
  cmb1 <- combinations(n=l_vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  return(sum(o[cmb1]))
}
