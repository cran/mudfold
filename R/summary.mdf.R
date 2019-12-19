summary.mdf <- function(object, boot=FALSE, type="perc", ...){
  if (is.null(boot)) boot <- FALSE

  opt1 <- object$CALL$Bootstrap == TRUE & boot == TRUE
  opt2 <- object$CALL$Bootstrap == FALSE & boot == TRUE
  opt3 <- object$CALL$Bootstrap == TRUE & boot == FALSE
  opt4 <- object$CALL$Bootstrap == FALSE & boot == FALSE

  if (opt1 ){
    return(summary_mdf_boot(object, type=type, ...))
  }
  if (opt2 ){
    warning("No bootstrap iterations for the summary. Try to set: boot = FALSE")
    return(summary_mdf(object))
  }
  if ((opt3 | opt4) ){
    return(summary_mdf(object))
  }
}