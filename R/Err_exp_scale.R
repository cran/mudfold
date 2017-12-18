Err_exp_scale <-function(X,vec=vec,exp=exp){
  l_vec <- length(vec)
  cmb1 <- combinations(n=l_vec, r=3, v=vec, set=FALSE, repeats.allowed=FALSE)
  return(sum(exp[cmb1]))
}