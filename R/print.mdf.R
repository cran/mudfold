print.mdf <- function(x, Item.info= FALSE, Diagnostics=FALSE, ...){
  cat("\nCall: ")
  print(x$call,...)
  cat("\nIndividuals:", x$sample.size,"\n")
  cat("\nItems:", x$no.items,"\n")
  cat("\nItem set:",x$starting.items,"\n")
  cat("Best unique triple:",x$Best.triple,"\n")
  cat("Iterations in the second step:",x$iterations.in.sec.step,"\n")
  cat("Unidimensional Mudfold scale:",x$mdfld.order,"\n")
  cat("Total number of observed errors for the Mudfold scale:",x$Obs.err.scale,"\n")
  cat("Total number of expected errors for the Mudfold scale:",x$Exp.err.scale,"\n")
  cat("scalability H for the Mudfold scale:",x$Htotal,"\n")
  cat("Iso statistic for the Mudfold scale:",x$Isototal,"\n")
  if (Item.info == TRUE){
    cat( paste(""), fill = TRUE )
    cat( paste( "Item Information:"  ), fill = TRUE )
    cat( paste(""), fill = TRUE )
    cat("Total number of observed errors per item in the Mudfold scale:",x$Obs.err.item,"\n")
    cat("Total number of expected errors per item in the Mudfold scale:",x$Exp.err.item,"\n")
    cat("scalability H per item in the Mudfold scale:",x$H.item,"\n")
    cat("Iso statistic per item in the Mudfold scale:",x$Item.ISO,"\n")
  }
  
  if (Diagnostics == TRUE){
    cat( paste(""), fill = TRUE )
    cat( paste( "Diagnostics:"  ), fill = TRUE )
    cat( paste(""), fill = TRUE )
    cat( paste( "Dominance matrix for Mudfold order:"  ), fill = TRUE )
    cat( paste(""), fill = TRUE )
    print(x$Dominance.matrix,...)
    cat( paste(""), fill = TRUE )
    cat( paste( "Adjacency matrix for Mudfold order:"  ), fill = TRUE )
    cat( paste(""), fill = TRUE )
    print(x$Adjacency.matrix,...)
    cat( paste(""), fill = TRUE )
    cat( paste( "Correlation matrix for Mudfold scale:"  ), fill = TRUE )
    cat( paste(""), fill = TRUE )
    print(x$Correlation.matrix,...)
    cat( paste(""), fill = TRUE )
    cat( paste( "Conditional Adjacency matrix for Mudfold scale: ", x$ISO  ), fill = TRUE )
    cat( paste(""), fill = TRUE )
    print(x$Cond.Adjacency.matrix,...)
  }
  
  cat("\n")
}

