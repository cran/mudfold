mudfold <- function(X=data, estimation="rank", lower.boundary=.3, start= NULL){
  if( !is.matrix(X) & !is.data.frame(X) ) stop( "Data should be a matrix or dataframe" )
  if( any(is.na(X))) stop("NA's not permitted")
  if( sum(!(X==0 | X==1)) > 0 ) stop("Data should be binary")
  strt.names <- colnames(X)
  if (is.null(strt.names)) strt.names <- LETTERS[1:ncol(X)]
  K<-ncol(X);n <- nrow(X);J <- strt.names;p <- apply(X,2,sum)/n;obs_frq <- apply(X, 2, sum)
  EO<-Err_exp(X);dimnames(EO) <- list(J,J,J)
  O<-Err_obs(X);dimnames(O) <- list(J,J,J)
  H_hjk<-Htrip(X);dimnames(H_hjk) <- list(J,J,J)
  if( any(is.na(H_hjk))) {
    print("MUDFOLD did not converge. NA's in scalability coefficients.")
    return()
  }
  if (!is.null(start))
  {
    n.itr <- length(J[!J %in% start])
    strt.indx <- start
    rmn.indx <- J[!J %in% start]
    unq <- NA
  }else{
    vec <- J
    g<-combinations(n=K, r=3, v=vec, set=FALSE, repeats.allowed=FALSE);g1<-g[,c(1,3,2)];g2<-g[,c(2,1,3)]
    Hhjk <- H_hjk[g];Hhkj <- H_hjk[g1];Hjhk <- H_hjk[g2]
    unq <- list(g[(Hhjk > 0 & Hhkj < 0 & Hjhk <0 ),], g1[(Hhjk < 0 & Hhkj > 0 & Hjhk < 0),], 
                g2[(Hhjk < 0 & Hhkj < 0 & Hjhk > 0),])
    if(sum(sapply(unq,nrow))==0) {
      print("MUDFOLD did not converge. No unique triple found.")
      return()
    }
    ctrp <- matrix(unlist(lapply(unq,function(x) as.vector(t(x)))),ncol = 3,byrow = T)
    Hunq <- H_hjk[ctrp]
    ord <- order(Hunq, decreasing = TRUE)
    if (max(Hunq) < lower.boundary) {
      print("MUDFOLD did not converge, H(unique) < lower boundary")
      return()
    }
    ord.unq.all <- ctrp[ord,]
    if (is.null(nrow(ord.unq.all))){
      bst.unq.trpl <- ord.unq.all
    }else{
      bst.unq.trpl <- ord.unq.all[1,]
    }
    strt.indx <- bst.unq.trpl
    rmn.indx <- J[!J %in% bst.unq.trpl]
    n.itr <- length(rmn.indx)
  }
  mdf.list <- list()
  for (e in 1 : n.itr){
    lstrt <- length(strt.indx);lrmn <- length(rmn.indx)
    indxi <- rep(seq(1,(lstrt+1)),lrmn);el <- rep(rmn.indx,each=lstrt+1)
    appd <- lapply(1:length(indxi), function(i) append(strt.indx,el[i],after = indxi[i]-1))
    cmbns <- lapply(1:max(1,length(appd)), function(i){ 
     a <- combinations(n=length(appd[[i]]),r=3, v=appd[[i]],set=FALSE, repeats.allowed=FALSE)
     b <- apply(a, 1, function(r) any(r %in% rmn.indx))
     c <- a[b,]
     })
    indc <- sapply(cmbns,function(x) sum(H_hjk[x]>0)==nrow(x))
    admit <- appd[indc]
    if (sum(indc)==0) break
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
      hi <- Hitem(df);names(hi)<- x
      index <- x[!x %in% strt.indx]
      cand.it <- as.numeric(hi[index])
      cand.it})
    Hs.max <- max(HS)
    if (Hs.max < lower.boundary)  break
    nfitr <- length(HS)
    for (x in 1 : nfitr) {
      if (HS[x] == Hs.max) {
        strt.indx <- nm[x,]
        rmn.indx <- J[!J %in% strt.indx]
      }
    }
  }
  df <- X[,strt.indx];colnames(df)<-strt.indx
  scale_length <- length(strt.indx)
  Cndadj <- CADJ(df);Corrlt <- cor(df);Adjcnc <- ADJ(df);Domnce <- DOM(df)
  items.rejected <- rmn.indx
  if (estimation=="rank"){
    estimates <- param_est(df,method = "rank")
    estimates$betas <- estimates$betas[strt.indx]
  }else{
    estimates <- param_est(df,method = "quantile")
    estimates$betas <- estimates$betas[strt.indx]
  }
  Obserr <- Err_obs_item(df);Experr <- Err_exp_item(df)
  Hitem1 <- Hitem(df);Isoitm <- ISO(Cndadj)
  Obsers <- Err_obs_scale(df);Expers <- Err_exp_scale(df);Hscals <- Hscale(df)
  Isotot <- sum(Isoitm);pj <- p[strt.indx]
  m1 <- list(dataf = X, starting.items = J, no.items=K, sample.size=n, Best.triple = bst.unq.trpl, iterations.in.sec.step = e,
             mdfld.order = strt.indx, length.scale=scale_length, item.popularities = pj, item.freq=obs_frq[strt.indx],
             Obs.err.item = Obserr, Exp.err.item = Experr,H.item = Hitem1 , Item.ISO = Isoitm , Obs.err.scale = Obsers,
             Exp.err.scale = Expers, Htotal = Hscals, Isototal = Isotot,Dominance.matrix = Domnce, 
             Adjacency.matrix = Adjcnc,Correlation.matrix = Corrlt, Cond.Adjacency.matrix = Cndadj, uniq= unq, 
             est.parameters=estimates, call=match.call())
  mdf.list <- m1
  class(mdf.list) <- "mdf"
  return(mdf.list)
}


