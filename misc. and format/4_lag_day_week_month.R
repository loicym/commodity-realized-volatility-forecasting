LagMean <- function(s, start.lag = c(1, 5, 22), end.lag = c(1, 1, 1)){
  
  n <- length(s)
  
  s.lag <- rep(NA, n)
  
  for(i in (start.lag + 1):n){
    
    s.lag[i] <- mean(s[(i - start.lag):(i - end.lag)], na.rm = T)
    
  }
  
  return(s.lag)
}

Lag <- function(s, start.lag = c(1, 5, 22)){
  
  n <- length(s)
  
  s.lag <- rep(NA, n)
  
  for(i in (start.lag + 1):n){
    
    s.lag[i] <- s[(i - start.lag)]
    
  }
  
  return(s.lag)
}