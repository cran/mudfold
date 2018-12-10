plot_persons <- function(x, ...){
  output <- x$MUDFOLD_INFO$second_step$scale
  nc <- x$MUDFOLD_INFO$second_step$Lscale
  bs <- x$MUDFOLD_INFO$second_step$estimates$betas
  #bn <- "bin"
  bn <- "..density.."
  th <- x$MUDFOLD_INFO$second_step$estimates$thetas
  df <- data.frame(th=th)
  pl <- NULL
  #pl <- ggplot(df,aes(x=th)) + geom_bar(stat=bn,bins = nc,color="darkblue", fill="lightblue")
  pl <- ggplot(df,aes_string(x=th,y=bn)) + geom_histogram(bins = nc,color="darkblue", fill="lightblue")
  pl <- pl +labs(title=expression(paste("Distribution of ",theta,"\'s (person parameters) on the latent scale",sep = " ")),
         x = "Latent scale",y = "")
  pl <- pl+ scale_x_continuous(breaks = seq(min(th,na.rm = TRUE),max(th,na.rm = TRUE),length.out = nc),labels=output)
  pl <- pl + theme_bw() + theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
                                axis.title = element_text(face = "bold",size = rel(1)),
                                axis.title.y = element_text(angle=90,vjust =2),
                                axis.title.x = element_text(vjust = -0.2),
                                axis.line = element_line(colour="black"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                legend.key = element_rect(colour = NA),
                                legend.position = "right",
                                legend.direction = "vertical",
                                legend.margin = margin(0, unit = "cm"),
                                legend.title = element_text(face="bold"),
                                plot.margin=unit(c(5,5,5,5),"mm"),
                                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                                strip.text = element_text(face="bold"))
  
  return(pl)
  
}