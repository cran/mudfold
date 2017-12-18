mudfold_se <- function(x,estimation=estimation,first.step=first.step,lambda1=lambda1,lambda2=lambda2,ls=ls){
  X<-x
  strt.indx <- as.character(first.step$strt.indx)
  rmn.indx <- first.step$rmn.indx
  n.itr <- first.step$n.itr
  K <- ls$K
  n <- ls$n
  J <- ls$J
  itr <- NULL
  for (e in 1 : n.itr){
    lstrt <- length(strt.indx);lrmn <- length(rmn.indx)
    indxi <- rep(seq(1,(lstrt+1)),lrmn)
    el <- rep(rmn.indx,each=lstrt+1)
    appd<-NULL
    for (i in 1:length(indxi)) appd[[i]] <- append(strt.indx,el[i],after = indxi[i]-1)
    cmbns<-lapply(appd,function(x) combinations(n=length(x), r=3, v=x, set=FALSE, repeats.allowed=FALSE))
    cmbns.chs <- lapply(cmbns,function(x) apply(x, 1, function(r) any(r %in% rmn.indx)))
    for (j in 1:max(1,length(cmbns))) cmbns[[j]] <- cmbns[[j]][cmbns.chs[[j]],]
    unl <- unlist(lapply(cmbns,function(x) sum(ls$H_hjk[x]>lambda2)==nrow(x)))
    admit <- appd[unl]
    if (sum(unl)==0) break
    admissible <- matrix(unlist(admit), ncol = lstrt+1, byrow = TRUE)
    freq.indx <- table(admissible)
    un.index <- names(freq.indx[which(freq.indx==min(freq.indx))])
    lngth <- length (un.index)
    mtind <- apply(admissible,1,function(r) any(r %in% un.index))
    nm <- matrix(admissible[mtind,],ncol = lstrt+1)
    nrnmt <- nrow(nm)
    if (is.null(nrnmt)) break
    HS <- apply(nm,1,function(x){
      df <- X[,x]
      hi <- Hitem(df,K=ncol(df),J=colnames(df),n=nrow(df),EO=ls$EO,O=ls$O)
      names(hi)<- x
      index <- x[!x %in% strt.indx]
      cand.it <- as.numeric(hi[index])
      cand.it})
    Hs.max <- max(HS)
    if (Hs.max < lambda1)  break
    nfitr <- length(HS)
    for (x in 1 : nfitr) {
      if (HS[x] == Hs.max) {
        strt.indx <- nm[x,]
        rmn.indx <- J[!J %in% strt.indx]
        itr <- e
      }
    }
  }
  data_end <- X[,strt.indx]
  K <- ncol(data_end)
  n <- nrow(data_end)
  J <- colnames(data_end)
  dimnames(data_end) <- list(1:n,strt.indx)
  scale_length <- length(strt.indx)
  pj <- ls$p[strt.indx]
  Cndadj <- CADJ(data_end,K=K,n=n,J=J)
  Corrlt <- cor(data_end)
  Adjcnc <- ADJ(data_end,K=K,n=n,J=J)
  Domnce <- DOM(data_end,K=K,n=n,J=J)
  items.rejected <- rmn.indx
  if (estimation=="rank"){
    estimates <- param_est(data_end,method = "rank")
    estimates$betas <- estimates$betas[strt.indx]
  }else{
    estimates <- param_est(data_end,method = "quantile")
    estimates$betas <- estimates$betas[strt.indx]
  }
  Obserr <- Err_obs_item(data_end,K=K,n=n,J=J,O=ls$O)
  Experr <- Err_exp_item(data_end,K=K,n=n,J=J,EO=ls$EO)
  Hitem1 <- Hitem(data_end,K=K,n=n,J=J,EO=ls$EO,O=ls$O)
  Isoitm <- ISO(Cndadj)
  Obsers <- Err_obs_scale(data_end,vec=J,o=ls$O)
  Expers <- Err_exp_scale(data_end,vec=J,exp=ls$EO)
  Hscals <- Hscale(data_end,vec=J,EO=ls$EO,O=ls$O)
  Isotot <- sum(Isoitm)
  star_mat <- CAM_STAR(Cndadj)
  m1 <- list(dat = x, starting.items = ls$J, no.items=ls$K, sample.size=ls$n, Best.triple = as.character(first.step$strt.indx), 
             iterations.in.sec.step = itr, mdfld.order = strt.indx, length.scale=scale_length, item.popularities = pj, item.freq=ls$obs_frq[strt.indx],
             Obs.err.item = Obserr, Exp.err.item = Experr,H.item = Hitem1 , Item.ISO = Isoitm , Obs.err.scale = Obsers,
             Exp.err.scale = Expers, Htotal = Hscals, Isototal = Isotot,Dominance.matrix = Domnce, star=star_mat, 
             Adjacency.matrix = Adjcnc,Correlation.matrix = Corrlt, Cond.Adjacency.matrix = Cndadj, uniq= first.step$unq.trp, 
             est.parameters=estimates,call=ls$call)
  class(m1) <- "mdf"
  return(m1)
}
