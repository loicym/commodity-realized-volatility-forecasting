TvCoeffPlot <- function(mod = mod.har.tv.ev
                        , df = df.min288
                        # , my.title = "Time Varying Intercepts of the EVHAR-TV model"
                        , my.ylab = "RV"){
  
  library(ggplot2)
  library(reshape2)
  
  my.time <- as.POSIXct(df$fin.date, origin = "1970-01-01")[523:2755]
  
  df.coeffs <- mod.har.tv.ev$coefficients
  
  print(colnames(df.coeffs))
  # print(grep("(Intercept)", colnames(df.coeffs)))
  
  df.intercept <- data.frame(df.coeffs[, grep("(Intercept)", colnames(df.coeffs))])

  names(df.intercept) <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  print(head(df.intercept))

  # dev.new()
  # matplot(df.intercept, t = 'l')
  
  df.intercept = cbind(my.time, df.intercept)
  
  print(head(df.intercept))
  
  
  colnames(df.intercept)[1] <- "date"
  print(head(date, 1))
  print(head(df.intercept, 1))
  
  ################
  #reshape
  
  #reshape to long format
  plot.df <- melt(df.intercept, id.var = "date")
  names(plot.df)[2] = "contracts"
  print(head(plot.df))
  print(tail(plot.df))
  print(dim(plot.df))
  
  # dev.new()
  g1 <- ggplot(plot.df, aes(x = date, y = value, group = contracts, linetype = contracts)) + geom_line()
  
  g1 <- g1 + ylab(my.ylab)
  
  g1 <- g1 + theme(axis.title.y = element_blank())
  g1 <- g1 + theme(axis.title.x = element_blank())
  g1 <- g1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
  
  g1 <- g1 + geom_line(data = plot.df, size = 1)
  
  # g1 <- g1 + coord_cartesian(ylim = c(0, max(plot.df$value, na.rm = T)))
  
  return(g1)
  
}