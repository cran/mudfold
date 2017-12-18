mudfold_fs <- function(x,lambda1=lambda1,ls=ls){
  X<-x
  g<-combinations(n=ls$K, r=3, v=ls$J, set=FALSE)
  g1<-g[,c(1,3,2)]
  g2<-g[,c(2,1,3)]
  Hhjk <- ls$H_hjk[g]
  Hhkj <- ls$H_hjk[g1]
  Hjhk <- ls$H_hjk[g2]
  unq.hjk <-  g[(Hhjk > 0 & Hhkj < 0 & Hjhk <0 ),]
  unq.hkj <- g1[(Hhjk < 0 & Hhkj > 0 & Hjhk < 0),]
  unq.jhk <- g2[(Hhjk < 0 & Hhkj < 0 & Hjhk > 0),]
  unq.all <- rbind (unq.hjk, unq.hkj, unq.jhk)
  lam <- nrow(unq.all)
  if( lam==0) {
    print("Data cannot be unfolded. No unique triple has been found.")
    return()
  }
  dimnames(unq.all) <- list(1:nrow(unq.all),c("Index1", "Index2", "Index3"))
  Hunq <- ls$H_hjk[unq.all]
  ord <- order(Hunq, decreasing = TRUE)
  names1 <- 1:lam
  if (max(Hunq) < lambda1) {
    print("Data cannot be unfolded, H(unique) < lower boundary")
    return()
  }
  ord.unq.all <- matrix(NA,nrow = nrow(unq.all), ncol = ncol(unq.all))
  dimnames(ord.unq.all) <- list(names1, c("v1","v2","v3"))
  ord.unq.all <- unq.all[ord,]
  if (is.null(nrow(ord.unq.all)))
    bst.unq.trpl <- ord.unq.all
  if (!is.null(nrow(ord.unq.all)))
    bst.unq.trpl <- ord.unq.all[1,]
  strtng.trpl <- bst.unq.trpl
  J.star <- ls$J[! ls$J %in% strtng.trpl]
  n.itr <- length(J.star)
  strt.indx <- strtng.trpl
  rmn.indx <- J.star
  return(list(strt.indx = strt.indx, rmn.indx =rmn.indx , n.itr = n.itr,
              unq.trp = ord.unq.all))
}
