#second function to format: get rid of sundays,
#compute time to maturity (in POSIXct time difference)
#compute the log scaled term structure
#function goes backward
#compute ret, squared ret and cross product (covariance)
FormatHf <- function(df){
  
  u.x <- dim(df)[1]
  p.X <- dim(df)[2] 
  df <- DeleteSundays(df)  
  df <- AddTimeMaturity(df)

  df <- AddScaledTermStructure(df)
  df <- AddReturns(df)
  df <- AddDate(df)
  
  return(df)
}

DeleteSundays <- function(df){
  
  print("with Sunday Df length is: ")
  print(dim(df)[1])
  time.stamp.day <- format(as.POSIXct(df$time.stamp1, origin = "1970-01-01"), format = "%a")
  df <- df[time.stamp.day != "Sun", ]
  
  print("with no Sunday Df length is: ")
  print(dim(df)[1])
  return(df)
}

AddTimeMaturity <- function(df){
  
  df <- na.omit(df)
  
  u.x <- dim(df)[1]
  p.x <- dim(df)[2] 
  
  time.stamp.vec <- df$time.stamp1
  symbol <- as.vector(df$symbol1)
  
  print(head(symbol))
  
  time.maturity <- rep(NA, u.x)
  k <- u.x
  
  for(i in u.x:2){
    
    if(symbol[i] != symbol[i - 1]){
      
      time.maturity[i - 1] <- 0
      ind.change <- i - 1
    }

    if(symbol[i] == symbol[i - 1]){
      
      time.maturity[i - 1] <- time.stamp.vec[k] - time.stamp.vec[i - 1]
    }
  }
  df <- data.frame(cbind(df, time.maturity))
  return(df)
}

AddScaledTermStructure <- function(df){
  
  u.x <- dim(df)[1]
  p.x <- dim(df)[2] 
  
  symbol1.year <- substr(df$symbol1, 4, 5)
  symbol2.year <- substr(df$symbol2, 4, 5)
  
  symbol1.month <- substr(df$symbol1, 3, 3)
  symbol2.month <- substr(df$symbol2, 3, 3)
  
  year1 <- paste0("20", symbol1.year)
  year2 <- paste0("20", symbol2.year)
  
  month1 <- convert_month(symbol1.month)
  month2 <- convert_month(symbol2.month)
  
  # print(month1)
  # print(year1)
  t.date1 <- as.Date(paste0(year1, ":", month1, ":", "01"), format = "%Y:%m:%d")
  t.date2 <- as.Date(paste0(year2, ":", month2, ":", "01"), format = "%Y:%m:%d")
  
  maturity.gap <- as.numeric(t.date2 - t.date1)
  log.term.struct <- (log(df$close2) - log(df$close1)) / maturity.gap
  # print(log.term.struct)
  
  df <- data.frame(cbind(df, maturity.gap, log.term.struct))
  return(df)
}

ConvertMonth <- function(vec.month){
  
  vec.month[vec.month == "F"] <- "01"
  vec.month[vec.month == "G"] <- "02"
  vec.month[vec.month == "H"] <- "03"
  vec.month[vec.month == "J"] <- "04"
  vec.month[vec.month == "K"] <- "05"
  vec.month[vec.month == "M"] <- "06"
  vec.month[vec.month == "N"] <- "07"
  vec.month[vec.month == "Q"] <- "08"
  vec.month[vec.month == "U"] <- "09"
  vec.month[vec.month == "V"] <- "10"
  vec.month[vec.month == "X"] <- "11"
  vec.month[vec.month == "Z"] <- "12"
  
 return(vec.month) 
}

AddReturns <- function(df){
  
  u.x <- dim(df)[1]
  p.x <- dim(df)[2]
  
  ret1 <- c(NA, log(df$close1[2:u.x]) - log(df$close1[1:(u.x - 1)]))
  ret2 <- c(NA, log(df$close2[2:u.x]) - log(df$close2[1:(u.x - 1)]))
  
  print(head(ret1))
  rv1 <- ret1^2
  rv2 <- ret2^2
  
  cov <- ret1 * ret2
  slrv <- (ret2 - ret1)^2
  
  #new addition: slope RV = RV SLRV
  
  df <- data.frame(cbind(df, ret1, ret2, rv1, rv2, cov, slrv))
  return(df)
}


AddDate <- function(df){
  
  my.date <- as.POSIXct(df$trading.day1, origin = "1970-01-01")
  df <- data.frame(cbind(my.date, df))
  return(df)
}
