coef.mdf <-  function(object, type, ...){
  if (missing(type)) type <-  "persons"
  if (type=="persons"){
    return(object$MUDFOLD_INFO$second_step$estimates$thetas)
  }
  if (type=="items"){
    return(object$MUDFOLD_INFO$second_step$estimates$betas)
  }
  if (type=="all"){
    return(list(persons= object$MUDFOLD_INFO$second_step$estimates$thetas,
                items = object$MUDFOLD_INFO$second_step$estimates$betas))
  }
  
}