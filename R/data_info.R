data_info <- function(X,call){
  K = ncol(X)
  n = nrow(X)
  J =colnames(X)
  p = apply(X,2,sum) / n
  obs_frq = apply(X, 2, sum)
  EO=Err_exp(X)
  O=Err_obs(X)
  H_hjk=Htrip(X,J=J,K=K,n=n,EO=EO,O=O)
  return(list(K=K,n=n,J=J,p=p,obs_frq=obs_frq,EO=EO,O=O,H_hjk=H_hjk,call=call))
}