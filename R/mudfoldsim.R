mudfoldsim <- function(items=8, sample.size=1000, pgamma1=5, pgamma2=-10, zeros=FALSE){
  if(zeros==TRUE){ 
    p <- items; n <- sample.size
    beta <- rnorm(p); theta <- rnorm(n)
    beta <-sort(beta); theta <- sort(theta)
    gamma1 <- pgamma1; gamma2 <- pgamma2; X <- Pma <- matrix(NA,n,p)
    for(i in 1:n){
      Pma[i,] <- (1+ exp(-gamma1-gamma2*(theta[i] - beta)^2))^{-1}
      X[i,] <- rbinom(p,1,Pma[i,])
    } 
    dimnames(X)[[2]] <- LETTERS[1:p]
    sim.data <-list(df=as.data.frame(X), item.patameters=beta,subject.parameters=theta)
    return(sim.data)
  }
  if(zeros==FALSE){ 
    p <- items; n <- 10*sample.size
    beta <- rnorm(p); theta <- rnorm(n)
    beta <-sort(beta); theta <- sort(theta)
    gamma1 <- pgamma1; gamma2 <- pgamma2; X <- Pma <- matrix(NA,n,p)
    for(i in 1:n){
      Pma[i,] <- (1+ exp(-gamma1-gamma2*(theta[i] - beta)^2))^{-1}
      X[i,] <- rbinom(p,1,Pma[i,])
    } 
    Amat<-cbind(X,theta);TotalScore<-apply(X,1,sum)
    Emat<-Amat[TotalScore!=0,]
    Kmat<-Emat[sample(nrow(Emat), sample.size), ]
    or<-order(Kmat[,p+1],decreasing = FALSE)
    X<-Kmat[or,1:p];dimnames(X)[[2]] <- LETTERS[1:p]
    X <- X[,sample(LETTERS[1:p])]
    sim.data <-list(df=as.data.frame(X), item.patameters=beta,subject.parameters=Kmat[or,p+1])
    return(sim.data)
  }
}
