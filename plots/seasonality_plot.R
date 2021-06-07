SeasonalPlot <- function(df = df.min288
                         , ticker = "C"
                         , ticker.name = "Corn"){
  
  library(reshape2)
  library(ggplot2)
  
  n.df <- dim(df)[1]
  p.df <- dim(df)[2]
  
  rv <- 0.5 * cleaner.vec(log(as.numeric(df[, paste0("rvar.288.", ticker)])))
  
  my.time <- as.POSIXct(df$fin.date, origin = "1970-01-01")
  
  #aggregate by month
  time.month <- as.numeric(format(my.time, format = "%m"))
  
  df.time <- data.frame(cbind(time.month, rv))
  
  agg.month.rv <- aggregate(df.time[, 2], list(df.time$time.month), mean, na.rm = T)

  ci.agg.month.rv <- aggregate(df.time[, 2], list(df.time$time.month), FUN = ConfInterval, ci.level = 0.1)
 
  plot.df <- data.frame(cbind(month.abb, agg.month.rv$x, agg.month.rv$x + ci.agg.month.rv$x, agg.month.rv$x - ci.agg.month.rv$x))
  
  plot.df[, 2:4] <- apply(plot.df[, 2:4], 2, as.double)# * 100
  
  names(plot.df) <- c("my.month.abb", "mean", "upr", "lwr")
  
  print(plot.df)
  dev.new()
  
  p1 <- ggplot(data = plot.df, aes(x = factor(my.month.abb, levels = month.abb), y = mean, colour = "monthly rv mean", group = 1))
    
  p1 <- p1 + geom_line(data = plot.df, size = 1)
  p1 <- p1 + geom_ribbon(data = plot.df, aes(ymin = upr, ymax = lwr), alpha = 0.2, colour = NA)
  
  p1 <- p1 + theme(axis.title.y = element_blank())
  p1 <- p1 + theme(axis.title.x = element_blank())
  p1 <- p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  p1 <- p1 + theme(legend.position = "none")+ theme(legend.position = "none")
  p1 <- p1 + ggtitle(ticker.name)
  
  p1 <- p1 + theme(plot.title = element_text(hjust = 0.5))
  
  p1 <- p1 + scale_y_continuous(labels = ScaleFunc)
  p1 <- p1 + xlab("Month") + ylab("Average RV")
  
  return(p1)
}


StdErr <- function(x){
  x <- na.omit(x)
  sd(x) / sqrt(length(x))
} 

ConfInterval <- function(x, ci.level = c(0.1, 0.05, 0.01)){
  ci.level <- 1 - ci.level / 2
  StdErr(x) * qnorm(ci.level)
}

ScaleFunc <- function(x){
  sprintf("%.2f", x)
}



SeasonalPlotJuly <- function(df = df.min288
                         , ticker = "NG"
                         , ticker.name = "Natural Gas"){
  
  library(reshape2)
  library(ggplot2)
  
  n.df <- dim(df)[1]
  p.df <- dim(df)[2]
  
  rv <- 0.5 * cleaner.vec(log(as.numeric(df[, paste0("rvar.288.", ticker)])))
  
  my.time <- as.POSIXct(df$fin.date, origin = "1970-01-01")
  
  #aggregate by month
  time.month <- as.numeric(format(my.time, format = "%m"))
  
  df.time <- data.frame(cbind(time.month, rv))
  
  agg.month.rv <- aggregate(df.time[, 2], list(df.time$time.month), mean, na.rm = T)
  
  ci.agg.month.rv <- aggregate(df.time[, 2], list(df.time$time.month), FUN = ConfInterval, ci.level = 0.1)

  plot.df <- data.frame(cbind(month.abb, agg.month.rv$x, agg.month.rv$x + ci.agg.month.rv$x, agg.month.rv$x - ci.agg.month.rv$x))
  
  plot.df <- rbind(plot.df[7:12, ], plot.df[1:6, ])
  
  plot.df[, 2:4] <- apply(plot.df[, 2:4], 2, as.double)# * 100
  
  names(plot.df) <- c("my.month.abb", "mean", "upr", "lwr")
  
  print(plot.df)
  dev.new()
  
  p1 <- ggplot(data = plot.df, aes(x = factor(c(month.abb[7:12], month.abb[1:6]), levels = c(month.abb[7:12], month.abb[1:6])), y = mean, colour = "monthly rv mean", group = 1))
  
  p1 <- p1 + geom_line(data = plot.df, size = 1)
  p1 <- p1 + geom_ribbon(data = plot.df, aes(ymin = upr, ymax = lwr), alpha = 0.2, colour = NA)
  
  p1 <- p1 + theme(axis.title.y = element_blank())
  p1 <- p1 + theme(axis.title.x = element_blank())
  p1 <- p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  p1 <- p1 + theme(legend.position = "none")+ theme(legend.position = "none")
  
  p1 <- p1 + ggtitle(ticker.name)
  
  p1 <- p1 + theme(plot.title = element_text(hjust = 0.5))
  
  p1 <- p1 + scale_y_continuous(labels = ScaleFunc)
  
  p1 <- p1 + xlab("Month") + ylab("Average RV")
  
  return(p1)
}
