CorrectTimeMaturity = function(df){
  
  tickers <- c("C","CL","GC","HG","HO","NG","S","SI","W")
  
  u.x <- dim(df)[1]

  #get the time stamps name vector
  t.s <- paste0("time.stamp1", tickers)
  
  #get the contract number
  switch <- paste0("symbol1", tickers)
  df.switch <- df[, switch]
  
  #delete all series of the data frame that are going backward in time
  df.ts <- df[, t.s]
  df.ts[is.na(df.ts)] <- 0
  
  #get the time to maturity names
  time.matu <- paste0("time.maturity", tickers)
  
  #create a matrix of false time to maturity
  df.time.matu <- df[, time.matu]
  df.time.matu[!is.na(df.time.matu)] <- NA
  print("ok")
  p.x <- dim(df.ts)[2]
  
  print(head(df.ts))
  for(j in 1:p.x){
    
    for(i in 2:u.x){
      
      # print(j)
      if(df.ts[i, j] < df.ts[i - 1, j])
      {
        df.ts[i, j] = df.ts[i - 1, j]
        print(paste("there was one for commodity", j, "at index", i))
      }
    }
  }
  
  df[, t.s] <- df.ts

  dev.new()
  matplot(df[, t.s], t = 'l')
  
  #replace the old time to maturity with the new one
  for(j in 1:p.x){
    
    ind <- u.x
    last.switch <- 0
    #starting backward:
    for(i in (u.x - 1):1){
      
      if(!is.na(df.switch[i, j]) && !is.na(df.switch[i + 1, j])){
        
        if(df.switch[i, j] < df.switch[i + 1, j]){
          
          ind <- i
          last.switch <- df.switch[i, j]
        }
        
        if(last.switch == 0 || df.ts[i, j] == 0){
          
          dfTimeMatu[i,j] = NA
          next
        }
        
        if(last.switch != 0)
        {
          df.time.matu[i, j] <- df.ts[ind, j] - df.ts[i, j]
        }
      }
    }
  }
  
  dev.new()
  matplot(df.time.matu, t = 'l')
  
  df[, time.matu] <- df.time.matu
  return(df)
}