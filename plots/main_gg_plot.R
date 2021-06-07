GgPlotMultiple <- function(df, y.base = "rv1C", y.pred = "multiHARpredC", my.title = "", my.ylab){
  
  library(reshape2)
  library(ggplot2)
  
  n.df <- dim(df)[1]
  p.df <- dim(df)[2]
  
  date <- as.POSIXct(df$myDateC, origin = "1970-01-01")
  
  print(tail(date))
  
  t.realized <- winsorize(df[, y.base], 0.005)
  t.forecast <- winsorize(df[, y.pred], 0.005)
  
  plot.df <- data.frame(cbind(date, t.realized, t.forecast))
  colnames(plot.df) <- c("date", "base", "pred")
  plot.df[, 1] <- date
  print(tail(plot.df, 2))
  # return()
  ################
  #reshape
  
  #reshape to long format
  # plot_y <- melt(y,id.var="date")
  # dev.new()
  
  p <- ggplot(plot.df, aes(x = date))
  p <- p + geom_line(aes(y = plot.df[, 2], colour = "realized"))
  p <- p + geom_line(aes(y = plot.df[, 3], colour = "forecast"))

  p <- p + ggtitle(myTitle)
  p <- p + theme(legend.position = "none")
  p <- p + theme(axis.title.y = element_blank())
  p <- p + theme(axis.title.x = element_blank())
  p <- p + scale_x_datetime(date_breaks = "3 years", date_labels = "%Y")
  
  p <- p + scale_y_continuous(labels = scaleFUN)
  
  # g1 + coord_cartesian(ylim=c(0, max(plot_y$value,na.rm=T)))
  
  return(p)
  # return()
}

#function to plot and organize plots on one page.
OrganizeGgPlot <- function(df, title, yLab){
  
  library(ggpubr)
  library(reshape2)
  library(ggplot2)
  
  tickers <- c("C", "CL", "GC", "HG", "HO", "NG", "S", "SI", "W")
  
  list.g <- list()
  dev.new()
  for(j in 1:9){
    
    realized <- paste0("rv1", tickers[j])
    forecast <- paste0("multiHARpredRv", tickers[j])
    # forecast <- paste0("uniHARpredRv", tickers[j])
    list.g[[j]] <- GgPlotMultiple(df, y.base = realized, y.pred = forecast, my.title = tickers[j], my.ylab = "realized volatility")
  }
  # print(c(list.g))
  
  g1 <- listG[[1]]
  g2 <- listG[[2]]
  g3 <- listG[[3]]
  g4 <- listG[[4]]
  g5 <- listG[[5]]
  g6 <- listG[[6]]
  g7 <- listG[[7]]
  g8 <- listG[[8]]
  g9 <- listG[[9]]
  
  print("ok")
  ggarrange(g1, g2, g3, g4, g5, g6, g7, g8, g9
            # , labels = tickers
            , ncol = 3, nrow = 3)
  
  # ggsave("financialization.eps")
}

ScaleFunc <- function(x){
  sprintf("%.2f", x)
}