as.mudfold <- function(x,estimation="rank"){
  call=match.call()
  X <- data_check(x)
  data_end <- X
  strt.indx <- colnames(data_end)
  info <- data_info(data_end,call=call)
  K <- info$K
  n <- info$n
  J <- info$J
  dimnames(data_end) <- list(1:n,strt.indx)
  scale_length <- length(strt.indx)
  pj <- apply(data_end,2, function(x) sum(x) / length(x))
  Cndadj <- CADJ(data_end,K=K,n=n,J=J)
  Corrlt <- cor(data_end)
  Adjcnc <- ADJ(data_end,K=K,n=n,J=J)
  Domnce <- DOM(data_end,K=K,n=n,J=J)
  if (estimation=="rank"){
    estimates <- param_est(data_end,method = "rank")
    estimates$betas <- estimates$betas[strt.indx]
  }else{
    estimates <- param_est(data_end,method = "quantile")
    estimates$betas <- estimates$betas[strt.indx]
  }
  Obserr <- Err_obs_item(data_end,K=K,n=n,J=J,O=info$O)
  Experr <- Err_exp_item(data_end,K=K,n=n,J=J,EO=info$EO)
  Hitem1 <- Hitem(data_end,K=K,n=n,J=J,EO=info$EO,O=info$O)
  Isoitm <- ISO(Cndadj)
  Obsers <- Err_obs_scale(data_end,vec=J,o=info$O)
  Expers <- Err_exp_scale(data_end,vec=J,exp=info$EO)
  Hscals <- Hscale(data_end,vec=J,EO=info$EO,O=info$O)
  Isotot <- sum(Isoitm)
  star_mat <- CAM_STAR(Cndadj)
  m1 <- list(dat = X, starting.items = info$J, no.items=info$K, sample.size=info$n, Best.triple = NA, 
             iterations.in.sec.step = NA, mdfld.order = strt.indx, length.scale=scale_length, item.popularities = pj, item.freq=info$obs_frq[strt.indx],
             Obs.err.item = Obserr, Exp.err.item = Experr,H.item = Hitem1 , Item.ISO = Isoitm , Obs.err.scale = Obsers,
             Exp.err.scale = Expers, Htotal = Hscals, Isototal = Isotot,Dominance.matrix = Domnce, star=star_mat, 
             Adjacency.matrix = Adjcnc,Correlation.matrix = Corrlt, Cond.Adjacency.matrix = Cndadj, uniq= NA, 
             est.parameters=estimates, call=info$call)
  class(m1) <- "mdf"
  return(m1)
}

