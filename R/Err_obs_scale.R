Err_obs_scale <-function(X,vec=vec,o=o){
  l_vec <- length(vec)
  cmb1 <- combinations(n=l_vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  return(sum(o[cmb1]))
}