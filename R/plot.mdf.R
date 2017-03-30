plot.mdf <-function(x,select=NULL, ...){
  output <- character(0L)
  output <- x$mdfld.order
  nc <- x$length.scale
  Items<-NA
  tcad <- t(x$Cond.Adjacency.matrix)
  value=variable=NULL
  A <- cbind(as.matrix(na.approx(tcad)),1:nc);dimnames(A)<- list(1:nc, c(output,"index"))
  A <- as.data.frame(A);d <- melt(A, id.vars="index");colnames(d) <- c("index","Items","value")
  
  if (is.null(select)){ 
    plot1 <- ggplot(d, aes(index,value, col=Items)) +
      geom_point() + geom_line()+ ylab("Probability of positive response")+
      ggtitle("Empirical Estimates for Item Response Curves ") +
      guides(col = guide_legend(keywidth = 2,keyheight = 1))
    plot1 <- plot1   +scale_x_discrete(name="Latent scale",limits=1:nc, labels=output)
    plot1 <- plot1+ theme_bw()  + theme(plot.title = element_text(face = "bold",
                                                                  size = rel(1.2), hjust = 0.5),
                                        axis.title = element_text(face = "bold",size = rel(1)),
                                        axis.title.y = element_text(angle=90,vjust =2),
                                        axis.title.x = element_text(vjust = -0.2),
                                        axis.line = element_line(colour="black"),
                                        panel.grid.major = element_line(colour="#f0f0f0"),
                                        legend.position = "right",
                                        legend.direction = "vertical",
                                        legend.key.size= unit(0.2, "cm"),
                                        legend.margin = margin(0, unit = "cm"),
                                        legend.title = element_text(face="bold"),
                                        
                                        plot.margin=unit(c(5,5,5,5),"mm"),
                                        strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                                        strip.text = element_text(face="bold")
    )
    
    return(plot1)
  }else{
    nd <- data.frame(subset(d,Items %in% select))
    plot1 <- ggplot(nd, aes(index,value, col=Items)) + ylim(0, 1)+
      geom_point() + geom_line()+ ylab("Probability of positive response")+
      ggtitle("Empirical Estimates for Item Response Curves ")
    plot1 <- plot1   +scale_x_discrete(name="Latent scale",limits=1:nc, labels=output)+ 
      guides(col = guide_legend(keywidth = 2,keyheight = 1))
    plot1 <- plot1 + theme_bw()+ theme(plot.title = element_text(face = "bold",
                                                                 size = rel(1.2), hjust = 0.5),
                                       axis.title = element_text(face = "bold",size = rel(1)),
                                       axis.title.y = element_text(angle=90,vjust =2),
                                       axis.title.x = element_text(vjust = -0.2),
                                       axis.line = element_line(colour="black"),
                                       panel.grid.major = element_line(colour="#f0f0f0"),
                                       legend.key = element_rect(colour = NA),
                                       legend.position = "right",
                                       legend.direction = "vertical",
                                       legend.key.size= unit(0.2, "cm"),
                                       legend.margin = margin(0, unit = "cm"),
                                       legend.title = element_text(face="bold"),
                                       plot.margin=unit(c(5,5,5,5),"mm"),
                                       strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                                       strip.text = element_text(face="bold")
    )
    return(plot1)
  }
}
