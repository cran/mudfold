mudfold <- function(x, estimation="rank", lambda1=.3, lambda2=0,start= NULL,check=TRUE){
  call=match.call()
  if (check==TRUE) x <- data_check(x) 
  info <- data_info(x,call=call)
  if( any(is.na(info$H_hjk))) {
    print("Data cannot be unfolded")
    return()
  }
  if (is.null(start)){
    first_step <- mudfold_fs(x,lambda1=lambda1,ls=info)
    if (is.null(first_step$n.itr)) return()
    sec_step <- mudfold_se(x,estimation=estimation,first.step=first_step,lambda1=lambda1,lambda2=lambda2,ls=info)
  }else{
    strtng.trpl <- start
    J.star <- info$J[! info$J %in% strtng.trpl]
    n.itr <- length(J.star)
    strt.indx <- strtng.trpl
    rmn.indx <- J.star
    unq <- NA
    first_step <- list(strt.indx = strt.indx, rmn.indx =rmn.indx , n.itr = n.itr,unq.trp = unq)
    sec_step <- mudfold_se(x,estimation=estimation,first.step=first_step,lambda1=lambda1,lambda2=lambda2,ls=info)
  }
  return(sec_step)
}
