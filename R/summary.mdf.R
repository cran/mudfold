summary.mdf <- function(object, boot=FALSE, diagnostics=FALSE, type="perc", ...){
  if (is.null(boot)) boot <- FALSE
  if (is.null(diagnostics)) diagnostics <- FALSE
  
  opt1 <- object$CALL$Bootstrap == TRUE & boot == TRUE
  opt2 <- object$CALL$Bootstrap == FALSE & boot == TRUE
  opt3 <- object$CALL$Bootstrap == TRUE & boot == FALSE
  opt4 <- object$CALL$Bootstrap == FALSE & boot == FALSE
  opt5 <- diagnostics
  
  if (opt1 & opt5){
    return(summary_mdf_boot(object,diagnostics = TRUE, type=type, ...))
  }
  if (opt1 & !opt5){
    return(summary_mdf_boot(object,diagnostics = FALSE, type=type, ...))
  }
  if (opt2 & opt5){
    warning("No bootstrap iterations for the summary. Try to set: boot = FALSE")
    return(summary_mdf(object,diagnostics = TRUE))
  }
  if (opt2 & !opt5){
    warning("No bootstrap iterations for the summary. Try to set: boot = FALSE")
    return(summary_mdf(object,diagnostics = FALSE))
  }
  if ((opt3 | opt4) & opt5){
    return(summary_mdf(object,diagnostics = TRUE))
  }
  if ((opt3 | opt4) & !opt5){
    return(summary_mdf(object,diagnostics = FALSE))
  }
}