Table2 <- function(list.min = list.1.min){
  
  library(stargazer)
  library(fracdiff)
  library(sandwich)
  library(lmtest)

  n <- dim(list.min$close1)[1]
  
  res.log.price.changes <- data.frame(matrix(data = NA, ncol = 9, nrow = 6))
  res.turnover <- data.frame(matrix(data = NA, ncol = 9, nrow = 6))
  res.volume <- data.frame(matrix(data = NA, ncol = 9, nrow = 6))
  res.count.trade <- data.frame(matrix(data = NA, ncol = 9, nrow = 6))
  res.bid.ask <- data.frame(matrix(data = NA, ncol = 9, nrow = 6))
  
  my.names <-  c("Corn (C)"
                 , "Soybeans (S)"
                 , "Wheat (W)"
                 , "WTI crude oil (CL)"
                 , "Heating oil (HO)"
                 , "Natural Gas (NG)"
                 , "Gold (GC)"
                 , "Copper (HG)"
                 , "Silver (SI)")
  
  colnames(res.log.price.changes) <- my.names
  colnames(res.turnover) <- my.names
  colnames(res.volume) <- my.names
  colnames(res.count.trade) <- my.names
  colnames(res.bid.ask) <- my.names

  
  log.price.changes <- log(list.min$close1[2:n, ] / list.min$close1[1:(n - 1), ])
  print(head(log.price.changes))

  bid.ask <- list.min$bid.ask
  #remove 0 and negative bid ask spread estimates
  bid.ask[bid.ask <= 0 ] <- NA
  
  count.trade <- list.min$count.trade
  #remove 0 trade days due to matching of contracts in the same time series with market closed
  count.trade[count.trade <= 0] <- NA
  
  for(j in 2:10){
    
    res.log.price.changes[, (j - 1)] <- InLineStats2(log.price.changes[, j] * 100)
    res.turnover[, (j - 1)] <- InLineStats2(list.min$turnover[, j] / 1000000)  #in million of USD
    res.volume[, (j - 1)] <- InLineStats2(list.min$volume[, j] / 1000) # in thousands of contracts
    res.count.trade[, (j - 1)] <- InLineStats2(count.trade[, j])
    res.bid.ask[, (j - 1)] <- InLineStats2(bid.ask[, j] * 10000) #in bps
    
  }
  print("ok")
  res <- data.frame(rbind(res.log.price.changes
                          , res.turnover
                          , res.volume
                          , res.count.trade
                          , res.bid.ask))

  print(res)
  
  stargazer(data.frame(rbind(res.log.price.changes
                             , res.turnover
                             , res.volume
                             , res.count.trade
                             , res.bid.ask))
                             , summary = FALSE, digits = 2, digits.extra = 2)
}

InLineStats2 <- function(vec){
  
  library(fBasics)
  
  n <- length(vec)[1]
  
  my.mean <- mean(vec, na.rm = T) #* 252                              #annualization of log price changes
  my.sd <- sd(vec, na.rm = T)# 252^0.5 * 100                          #annualization of sd
  
  my.skew <- skewness(vec, na.rm = T) #* 100 # / 252^0.5              #annualization of skewness
  my.kurto <- kurtosis(vec, na.rm = T) #* 100 #/ 252                  #annualization of excess kurtosis (default method)
  
  my.max <- max(vec, na.rm = T)
  my.min <- min(vec, na.rm = T)
  
  return(c(my.mean, my.sd, my.skew, my.kurto, my.min, my.max))
}