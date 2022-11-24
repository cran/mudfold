plot.mdf <-function(x, select, plot.type, ...){
  null.plot <- missing(plot.type)
  null.sellect <- missing(select)
  if (null.sellect) select <- NULL
  if (null.plot) plot.type <- "IRF"
  plottype1 <- plot.type=="IRF"
  plottype2 <- plot.type=="scale"
  plottype3 <- plot.type=="persons"
  
  if (!plot.type %in% c("IRF","scale","persons")) stop("Provide a valid type of plot. Available options are: IRF, scale, persons")
  if (plottype1) p <- plot_irf(x=x,select=select,...)
  if (plottype2) p <- plot_scale(x=x,select=select,...)
  if (plottype3) p <- plot_persons(x=x,select=NULL,...)
  
  return(p)
}
