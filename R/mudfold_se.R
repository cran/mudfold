mudfold_se <- function(out){
  out_list <- out
  data <- out_list$CALL$data
  lambda1 <- out_list$CALL$lambda1
  lambda2 <- out_list$CALL$lambda2
  hcoeft <- out_list$MUDFOLD_INFO$triple_stats$H_coefficients
  obserr <- out_list$MUDFOLD_INFO$triple_stats$Observed_errors
  experr <- out_list$MUDFOLD_INFO$triple_stats$Expected_errors
  estimation <- out_list$CALL$estimation
  
  N <- out_list$DESCRIPTIVES$n_items_final
  I <- out_list$DESCRIPTIVES$item_names_final 
  
  list_res <- list()
  list_res <- out_list
  list_res$MUDFOLD_INFO$second_step <-list()
  list_res$MUDFOLD_INFO$second_step$Converged <- FALSE
  if (is.null(list_res$CALL$start)) b_unq <- list_res$MUDFOLD_INFO$first_step$BU[1,]
  
  if (!is.null(list_res$CALL$start)){
    list_res$MUDFOLD_INFO$first_step$Converged <- "SKIPPED"
    b_unq <- list_res$CALL$start
    list_res$MUDFOLD_INFO$first_step$BU <- b_unq
  } 
  if (is.null(b_unq)) return()
  
  reit <- I[!I %in% b_unq]
  Nstar <- length(b_unq)
  for (iter in 1:(N-Nstar)){
    ## Create indices to be used in constructing scales
    index_rep <- rep(seq(1,(length(b_unq)+1)),length(reit))
    index_irep <- rep(reit, each=length(b_unq)+1)
    
    ## Construct the possible  scales to be investigated in each "iter"
    all_scales <- lapply(1:length(index_rep), 
                         function(i) append(b_unq,index_irep[i],after = index_rep[i]-1))
    
    ## Check which scales fulfill the first criterion.
    first_criterion <- lapply(all_scales, function(x){
      cmbnts <- combinations(n=length(x), r=3, v=x, set=FALSE, repeats.allowed=FALSE)
      indexes <- apply(cmbnts,1, function(y) any(y %in% reit))
      used_cmbs <- cmbnts[indexes,]
      if(sum(hcoeft[used_cmbs] > lambda2) == nrow(used_cmbs)) return(x) 
    })
    first_criterion <- first_criterion[!sapply(first_criterion,is.null)]
    
    ## If no scales fulfill the first criterion stop.
    if (length(first_criterion) ==0) break # If FALSE
    
    ## Check if scales fulfill the second criterion.
    min_seccri <- names(table(unlist(first_criterion))[table(unlist(first_criterion))==min(table(unlist(first_criterion)))])
    indexes <- sapply(first_criterion,function(x) any(x %in% min_seccri))
    second_criterion <- first_criterion[indexes]
    
    ## If no scales fulfill the second criterion stop.
    if (length(second_criterion) ==0) break # If FALSE
    
    ## Check if scales fulfill the third criterion.
    thirdcri <- sapply(second_criterion,function(x){
      Hitem(data[,x],EO=experr, O=obserr)[!x%in%b_unq]
    })
    
    ## If no scales fulfill the third criterion stop.
    if (max(thirdcri) < lambda1) break # If FALSE
    which_max_h <- which(thirdcri==max(thirdcri))
    
    ## Additional step
    if (length(which_max_h) > 1){
      crit3 <- second_criterion[which_max_h]
      tcrit3 <- table(unlist(crit3))
      icrit3 <- names(tcrit3)[tcrit3 == min(tcrit3)]
      lcrit3 <- sapply(crit3,function(x) any(icrit3 %in% x))
      if (sum(lcrit3) == 1) third_criterion <- crit3[lcrit3]
      if (sum(lcrit3) > 1){
        crit31 <- sapply(crit3[lcrit3],function(x) Err_exp_item(data[,x],EO=experr)[!x%in%b_unq])
        third_criterion <- crit3[lcrit3][which.min(crit31)]
      }
    }else{
      third_criterion <- second_criterion[which_max_h]
    }
    
    ## Determine best scale in each iter and items remaining.
    b_unq <- third_criterion[[1]]
    reit <- reit[!reit%in%b_unq]
    list_res$MUDFOLD_INFO$second_step$Converged <- TRUE
  }
  list_res$MUDFOLD_INFO$second_step$scale <- b_unq
  list_res$MUDFOLD_INFO$second_step$excluded_items <- reit
  list_res$MUDFOLD_INFO$second_step$Lscale <- length(b_unq)
  list_res$MUDFOLD_INFO$second_step$COND_ADJ <- CADJ(data[,b_unq])
  list_res$MUDFOLD_INFO$second_step$CORR <- cor(data[,b_unq])
  list_res$MUDFOLD_INFO$second_step$ADJ <- ADJ(data[,b_unq]) 
  list_res$MUDFOLD_INFO$second_step$DOM <- DOM(data[,b_unq])
  list_res$MUDFOLD_INFO$second_step$STAR <- CAM_STAR(list_res$MUDFOLD_INFO$second_step$COND_ADJ)
  list_res$MUDFOLD_INFO$second_step$Hscale <- Hscale(data[,b_unq],EO=experr,O=obserr)
  list_res$MUDFOLD_INFO$second_step$Hitem <- Hitem(data[,b_unq],EO=experr,O=obserr)
  #list_res$MUDFOLD_INFO$second_step$H_minus_item <- Hscalej(data[,b_unq],EO=experr,O=obserr)
  list_res$MUDFOLD_INFO$second_step$ISOitem <- ISO(list_res$MUDFOLD_INFO$second_step$COND_ADJ)
  list_res$MUDFOLD_INFO$second_step$ISOscale <- sum(list_res$MUDFOLD_INFO$second_step$ISOitem)
  list_res$MUDFOLD_INFO$second_step$OBSitem <- Err_obs_item(data[,b_unq],O=obserr)
  list_res$MUDFOLD_INFO$second_step$OBSscale<- Err_obs_scale(data[,b_unq],O=obserr)
  list_res$MUDFOLD_INFO$second_step$EXPitem <- Err_exp_item(data[,b_unq],EO=experr)
  list_res$MUDFOLD_INFO$second_step$EXPscale<- Err_exp_scale(data[,b_unq],EO=experr)
  if (estimation=="rank"){
    list_res$MUDFOLD_INFO$second_step$estimates <- param_est(data[,b_unq],method = "rank")
    list_res$MUDFOLD_INFO$second_step$estimates$betas <- list_res$MUDFOLD_INFO$second_step$estimates$betas[b_unq]
  }else{
    list_res$MUDFOLD_INFO$second_step$estimates <- param_est(data[,b_unq],method = "quantile")
    list_res$MUDFOLD_INFO$second_step$estimates$betas <- list_res$MUDFOLD_INFO$second_step$estimates$betas[b_unq]
  }
  class(list_res) <- "mdf"
  return(list_res)
}
