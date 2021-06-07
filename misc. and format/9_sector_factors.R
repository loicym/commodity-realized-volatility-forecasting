#match all HF data and take only first price, date and time stamp to compute factors
#argument: takes all df in list by alphabetical order
SectorFactorsHf <- function(my.list){
  
  n.list <- length(my.list)
  
  #first concatenate all POSIX information:
  time.stamp1 <- NA
  
  for(i in 1:n.list){
    time.stamp1 <- c(time.stamp1, my.list[[i]]$time.stamp1)
  }
  
  time.stamp1 <- time.stamp1[!is.na(time.stamp1)]
  #then create a factor variable and sort the occurences and get the vector from this factor
  time.stamp.all <- as.numeric(levels(as.factor(time.stamp1)))
  
  
  n.all <- length(time.stamp.all)
  
  print(n.all)
  
  hf.df <- data.frame(matrix(data = NA, nrow = n.all, ncol = 9 * 8))
  
  # return()
  k <- 1
  
  for(i in 1:n.list){
    
    print(i)
    t.match <- match(time.stamp.all, my.list[[i]]$time.stamp1)
    
    # print(t.match)
    print(length(t.match))
    # return()
    hf.df[k:(k + 7)] <- cbind(my.list[[i]][t.match, "my.date"]
                                , my.list[[i]][t.match, "time.stamp1"]
                                , my.list[[i]][t.match, "close1"]
                                , my.list[[i]][t.match, "volume1"]
                                , my.list[[i]][t.match, "close2"]
                                , my.list[[i]][t.match, "volume2"]
                                , my.list[[i]][t.match, "time.maturity"]
                                , my.list[[i]][t.match, "log.term.struct"])

    names(hf.df)
    k <- k + 8
  }
  
  my.seq <- seq(1, 72, by = 8)
  t.trading.day <- rowMeans(hf.df[, my.seq], na.rm = T)
  
  my.seq.dates.rm <- seq(1, 72, by = 8)
  
  hf.df <- hf.df[, -c(my.seq.dates.rm, my.seq.dates.rm + 1)]
  tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  k <- 1
  
  for(i in 1:9){
    names(hf.df)[k:(k + 5)] <- c(paste0("close1.", tickers[i])
                               , paste0("volume1.", tickers[i])
                               , paste0("close2.", tickers[i])
                               , paste0("volume2.", tickers[i])
                               , paste0("time.maturity.", tickers[i])
                               , paste0("log.term.struct.", tickers[i]))
    k <- k + 6
  }
  
  names.hf.df <- names(hf.df)
  
  hf.mat <- data.matrix(hf.df)
  print(head(hf.mat))
  
  for(j in 1:dim(hf.mat)[2]){
    print(j)
    
    if(substring(names.hf.df[j], 1, 4) == "clos" || substring(names.hf.df[j], 1, 4) == "log." || substring(names.hf.df[j], 1, 4) == "time"){
      for(i in 2:dim(hf.mat)[1]){
        
        if(i%%100000 == 0)
          print(i)
        
        if(is.na(hf.mat[i, j])){
          hf.mat[i, j] <- hf.mat[(i - 1), j]
        }
      }
    }
           
    if(substring(names.hf.df[j], 1, 4) == "volu"){
      for(i in 1:dim(hf.mat)[1]){
        
        if(i%%100000 == 0)
          print(i)
        
        if(is.na(hf.mat[i, j])){
          hf.mat[i, j] <- 0
      }
    }
  }
}
  hf.df <- data.frame(hf.mat)
  names(hf.df) <- names.hf.df
  return(data.frame(cbind(time.stamp.all, t.trading.day, hf.df)))
}