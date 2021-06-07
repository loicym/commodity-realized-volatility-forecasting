AddSlopeSupInf <- function(my.df){
  
  u.x <- dim(my.df)[1]
  p.x <- dim(my.df)[2]
  
  my.tickers <- c("C","CL","GC","HG","HO","NG","S","SI","W")
  
  slope.name <- "log.term.struct"
  
  out.df.sup <- data.frame(matrix(data = NA, nrow = uX, ncol = 9))
  out.df.inf <- data.frame(matrix(data = NA, nrow = uX, ncol = 9))
  
  for(j in 1:9){
    
    my.name <- paste0(slope.name, my.tickers[j])
    
    slope.vec <- my.df[, my.name]
    
    sl.sup <- slope.vec
    sl.inf <- slope.vec
    
    sl.sup[slope.vec < 0] <- 0
    sl.inf[slope.vec > 0] <- 0
    
    out.df.sup[, j] <- sl.sup
    out.df.inf[, j] <- sl.inf
    
    colnames(out.df.sup)[j] <- paste0("sl.sup", my.tickers[j])
    colnames(out.df.inf)[j] <- paste0("sl.inf", my.tickers[j])
  }
  return(data.frame(cbind(my.df, out.df.sup, out.df.inf)))
}