Err_obs_scale <-function(X,O){
  s <- colnames(X)
  l_vec <- length(s)
  cmb1 <- combinations(n=l_vec, r=3, v=s, set=FALSE, repeats.allowed=FALSE)
  return(sum(O[cmb1]))
}
