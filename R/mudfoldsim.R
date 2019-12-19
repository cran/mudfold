mudfoldsim <- function(N, n, gamma1=5, gamma2=-10, zeros=FALSE, parameters="normal", seed=NULL){
  if (!is.null(seed)){
    set.seed(seed)
  }
  if (is.null(N)) stop("The argument N (number of items) must be provided")
  if (is.null(n)) stop("The argument n (number of persons) must be provided")
  if (is.null(gamma1)) gamma1 <- 5
  if (is.null(gamma2)) gamma1 <- -10
  if (is.null(zeros)) zeros <- FALSE
  if (is.null(parameters)) parameters <- "normal"
  
  if(zeros==TRUE){ 
    beta <- rnorm(N);beta <-sort(beta)
    if (parameters=="normal") theta <- rnorm(n)
    if (parameters=="uniform") theta <- runif(n,min = min(beta),max = max(beta))
    theta <- sort(theta)
    gamma1 <- gamma1; gamma2 <- gamma2; X <- Pma <- matrix(NA,n,N)
    for(i in 1:n){
      Pma[i,] <- (1+ exp(-gamma1-gamma2*(theta[i] - beta)^2))^{-1}
      X[i,] <- rbinom(N,1,Pma[i,])
    } 
    rnk <- 1:N
    dim2 <- NA
    if (N<= 26) dim2 <- LETTERS[rnk] else dim2 <- as.character(rnk)
    dimnames(X)[[2]] <- dim2
    dimnames(Pma)[[2]] <- dim2
    names(rnk) <- dim2
    nam <- sample(dim2)
    X <- X[,nam]
    Prob <- Pma[,nam]
    sim.data <-list(obs_ord=nam,true_ord=dim2,items=N, sample=n,gamma1=gamma1,gamma2=gamma2,seed=seed,
                    dat=as.data.frame(X),probs=Prob,item.patameters=beta[as.numeric(rnk[nam])],subject.parameters=theta)
    return(sim.data)
  }
  if(zeros==FALSE){ 
    n1 <- n
    n <- 10*n
    beta <- rnorm(N);beta <-sort(beta)
    if (parameters=="normal") theta <- rnorm(n)
    if (parameters=="uniform") theta <- runif(n,min = min(beta),max = max(beta))
    theta <- sort(theta)
    X <- Pma <- matrix(NA,n,N)
    for(i in 1:n){
      Pma[i,] <- (1+ exp(-gamma1-gamma2*(theta[i] - beta)^2))^{-1}
      X[i,] <- rbinom(N,1,Pma[i,])
    } 
    Amat<-cbind(X,theta)
    Prob <- Pma
    TotalScore<-apply(X,1,sum)
    indx0 <- which(TotalScore!=0)
    Emat <- Amat[indx0,]
    Prob <- Pma[indx0,]
    sampling <- sample(nrow(Emat), n1)
    Kmat <- Emat[sampling, ]
    Prob <- Prob[sampling,]
    or<-order(Kmat[,N+1],decreasing = FALSE)
    rnk <- 1:N
    X<-Kmat[or,rnk]
    Prob <- Prob[or,]
    dim2 <- NA
    if (N<= 26) dim2 <- LETTERS[rnk] else dim2 <- paste("Item",as.character(rnk), sep = "")
    dimnames(X)[[2]] <- dim2
    dimnames(Prob)[[2]] <- dim2
    names(rnk) <- dim2
    nam <- sample(dim2)
    X <- X[,nam]
    sim.data <-list(obs_ord=nam,true_ord=dim2,items=N, sample=n1,gamma1=gamma1,gamma2=gamma2,seed=seed,
                    dat=as.data.frame(X),probs=Prob[,nam], item.patameters=beta[as.numeric(rnk[nam])],
                    subject.parameters=Kmat[or,N+1])
    return(sim.data)
  }
}
