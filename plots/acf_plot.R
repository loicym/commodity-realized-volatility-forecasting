AcfPlot <- function(s.acf, ticker.name, model.name){
  
library(reshape2)
library(ggplot2)

n <- length(s.acf)

lag <- 1:n

plot.df <- data.frame(cbind(s.acf, lag))

names(plot.df) <- c("acf", "displacement")

print(head(plot.df))
dev.new

p1 <- ggplot(data = plot.df, aes(x = displacement, y = acf, colour = "acf", group = 1))

p1 <- p1 + geom_line(data = plot.df, size = 1)
# p1 <- p1 + geom_ribbon(data = plot.df, aes(ymin = upr, ymax = lwr), alpha = 0.2, colour = NA)

p1 <- p1 + theme(axis.title.y = element_blank())
p1 <- p1 + theme(axis.title.x = element_blank())
p1 <- p1 + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p1 <- p1 + theme(legend.position = "none")+ theme(legend.position = "none")
# p1 <- p1 + ggtitle(paste0("Autocorrelation of residuals of: ", model.name, " model and commodity contract: ", ticker.name))

p1 <- p1 + theme(plot.title = element_text(hjust = 0.5))

p1 <- p1 + scale_y_continuous(labels = ScaleFunc)
p1 <- p1 + xlab("Displacement") + ylab("Autocorrelation")

return(p1)
}

AcfFun <- function(s, lag){
  
  n <- length(s)
  my.acf <- rep(NA, lag - 1)
  for(i in 2:lag){
    
    my.acf[i - 1] <- cor(s[i:n], s[1 : (n - i + 1)])

  }
  
# dev.new()
# plot(my.acf, t = 'l')
return(my.acf)
}