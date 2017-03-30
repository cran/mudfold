summary.mdf <-function(object,...){
  item.index <- object$mdfld.order
  K<-object$length.scale
  pj<-round(object$item.popularities,2)
  nr <- object$sample.size
  pj_c <- 1 - pj  
  stdd<- round(sqrt((pj*(1-pj_c))/nr),2)
  fre<- object$item.freq
  Exp.errors<-round(object$Exp.err.item, 2);Exp.errors.tot <- round(object$Exp.err.scale,2)
  Obs.errors<-round(object$Obs.err.item, 2);Obs.errors.tot <- round(object$Obs.err.scale,2)
  Hj<-round(object$H.item,2); Htot <- round(object$Htotal,2)
  Iso.stat<-round(object$Item.ISO,2);Iso.stat.tot <- round(object$Isototal,2)
  summ_ob <- matrix(NA, nrow=K+1, ncol=9)
  dimnames(summ_ob) <- list(1:(K+1), as.character(c("index", "samp.size", "freq", "prop", "std.err", "Obs.err", "Exp.err", "Iso","Scalab.H" )))
  summ_ob <- as.data.frame(summ_ob)
  summ_ob$index <-factor(c(item.index, "total"),levels = c(item.index, "total")) 
  summ_ob$samp.size <- rep(nr,K+1)
  summ_ob$freq <- c(fre, NA)
  summ_ob$prop <- c(pj, NA)
  summ_ob$std.err <- c(stdd, NA)
  summ_ob$Obs.err <- c(Obs.errors, Obs.errors.tot)
  summ_ob$Exp.err <- c(Exp.errors, Exp.errors.tot)
  summ_ob$Scalab.H <- c(Hj, Htot)
  summ_ob$Iso <- c(Iso.stat, Iso.stat.tot)
  return(summ_ob)
}


