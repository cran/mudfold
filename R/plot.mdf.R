plot.mdf <-function(x,select=NULL,plot.type="IRF",...){
  null.plot <- is.null(plot.type)
  plottype1 <- plot.type=="IRF"
  plottype2 <- plot.type=="scale"
  if (null.plot) return("Please provide a type of plot. Available options are IRF plot and scale plot.")
  if (plottype1) p <- plot_irf(x=x,select=select,...)
  if (plottype2) p <- plot_scale(x=x,select=select,...)
  return(p)
}