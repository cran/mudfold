summary_mdf <- function(object,...){
  summaryobject <- list()
  dat <- object$CALL$data
  item.index <- object$MUDFOLD_INFO$second_step$scale
  
  si <- object$DESCRIPTIVES$n_items_final
  sn <- object$DESCRIPTIVES$item_names_final
  
  K <- object$MUDFOLD_INFO$second_step$Lscale
  pj<- object$DESCRIPTIVES$prop_per_item[item.index]
  nr <- object$DESCRIPTIVES$n_persons_final
  pj_c <- 1 - pj  
  stdd <- round(sqrt((pj*(1-pj_c))/nr),2)
  fre <- object$DESCRIPTIVES$positives_per_item[item.index]
  Exp.errors<-round(object$MUDFOLD_INFO$second_step$EXPitem, 2)
  Exp.errors.tot <- round(object$MUDFOLD_INFO$second_step$EXPscale,2)
  Obs.errors <- round(object$MUDFOLD_INFO$second_step$OBSitem, 2)
  Obs.errors.tot <- round(object$MUDFOLD_INFO$second_step$OBSscale,2)
  
  Hj <- round(object$MUDFOLD_INFO$second_step$Hitem,2)
  Htot <- round(object$MUDFOLD_INFO$second_step$Hscale,2)
  Iso.stat <- round(object$MUDFOLD_INFO$second_step$ISOitem,2)
  Iso.stat.tot <- round(object$MUDFOLD_INFO$second_step$ISOscale,2)
  Max.stat <- round(object$MUDFOLD_INFO$second_step$MAXitem,2)
  Max.stat.tot <- round(object$MUDFOLD_INFO$second_step$MAXscale,2)
  
  summs <- matrix(NA, nrow=5, ncol=1)
  dimnames(summs)[[1]] <-c("H(scale)","ISO(scale)","MAX(scale)", "O(scale)", "EO(scale)")
  dimnames(summs)[[2]] <- "value"
  summs <- as.data.frame(summs)
  summs[1,] <- Htot
  summs[2,] <- Iso.stat.tot
  summs[3,] <- Max.stat.tot
  summs[4,] <- Obs.errors.tot
  summs[5,] <- Exp.errors.tot
  summaryobject$SCALE_STATS <- summs
  
  
  summ <- matrix(NA, nrow=K, ncol=10)
  dimnames(summ)[[2]] <-c("items", "n_persons", 
                          "posit(items)", "pposit(items)", "se(items)",
                          "O(items)", "EO(items)","MAX(items)", "ISO(items)","H(items)" )
  
  summ <- as.data.frame(summ)
  summ[,1] <- item.index
  summ[,2] <- rep(nr,K)
  summ[,3] <- fre
  summ[,4] <- pj
  summ[,5] <- stdd
  summ[,6] <- Obs.errors
  summ[,7] <- Exp.errors
  summ[,8] <- Max.stat
  summ[,9] <- Iso.stat
  summ[,10] <- Hj
  summaryobject$ITEM_STATS <- summ

  return(summaryobject)
  
}