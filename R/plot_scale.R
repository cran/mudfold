plot_scale <- function(x,select=NULL,...){
  lscale <- x$length.scale
  is.select <- is.null(select)
  itm.rank <- 1:lscale
  plot.size <- 0:(lscale+1)
  plot.labs <- x$mdfld.order
  if (is.select){
    df <- data.frame()
    p <- ggplot(df)+xlim(0, lscale+1)+ylim(-0.5,0.5)
    p <- p + geom_line(aes(x=plot.size,y=-0.2),size=1.5) +geom_point(aes(x=itm.rank,y=-0.2),size=3)
    p <- p + geom_text( nudge_y = 0,aes(x=itm.rank,y=0,label=plot.labs),angle=90)
    p <- p +ggtitle("Unidimensional MUDFOLD scale")
    p <- p + theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
              axis.line = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
    return(p)
  }else{
    num <- is.numeric(select)
    chr <- is.character(select)
    if (num){itm.rank <- itm.rank[select]}
    if (chr){itm.rank <- itm.rank[which(plot.labs %in% select)]}
    plot.labs <- plot.labs[itm.rank]
    df <- data.frame()
    p <- ggplot(df)+xlim(0, lscale+1)+ylim(-0.5,0.5)
    p <- p + geom_line(aes(x=plot.size,y=-0.2),size=1.5) +geom_point(aes(x=itm.rank,y=-0.2),size=3)
    p <- p + geom_text( nudge_y = 0,aes(x=itm.rank,y=0,label=plot.labs),angle=90)
    p <- p +ggtitle("Unidimensional MUDFOLD scale")
    p <- p + theme(plot.title = element_text(hjust = 0.5),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), 
                   axis.line = element_blank(),axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                   axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
    return(p)
  }

  
}
?geom_line
