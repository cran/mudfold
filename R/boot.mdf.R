boot.mdf <- function(data,nboot,start,lambda1,lambda2,estimation,...){
  return(boot(data,STATSboot,R=nboot,start=start,lambda1=lambda1,lambda2=lambda2,estimation =estimation,...))
}