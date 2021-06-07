Main <- function(end.path)
{
  my.path <- paste0("C:/Users/loicy/Dropbox/unine/loic/thesis/R/RProj2 - rv/data/1 min/", end.path)
  
  setwd(my.path)
  
  my.files <- list.files(my.path)
  print(my.files)
  print(length(my.files))
   
  my.list <- FileReader(my.path, my.files)
  
  n.list <- length(my.list)
  print("ok")
  
  for(i in 1:n.list){
    print(i)
    my.list[[i]] <- FileFormater(my.list[[i]])
    print(head(my.list[[i]]))
  }
  
  # return(my.list)
  
  
  t.list <- FileMatcher(my.list[[1]], my.list[[2]])
  fin.df <- t.list$my.df
  
  
  last.time.stamp <- t.list$last.time.stamp
  for(i in 2:(n.list - 1)){
    
    print(paste0("file matcher index is: ", i))
    
    print(tail(as.vector(my.list[[i + 1]]$time.stamp), 1))
    
    if(last.time.stamp >= tail(as.vector(my.list[[i + 1]]$time.stamp), 1))
      break
    
    t.list <- FileMatcher(my.list[[i]], my.list[[i + 1]], last.time.stamp)
    
    print("ok file matcher")
    
    fin.df <- rbind(fin.df, t.list$my.df)
    print(dim(fin.df))
    last.time.stamp <- t.list$last.time.stamp
  }
  
  colnames(fin.df) <- c("symbol1", "time.stamp1", "trading.day1", "close1", "volume1"
                       , "symbol2", "time.stamp2", "trading.day2", "close2", "volume2")
  return(fin.df)
}


MainFromList <- function(my.list){
  
  n.list <- length(my.list)
  t.list <- FileMatcher(my.list[[1]], my.list[[2]])
  fin.df <- t.list$my.df
  
  # return(my.list)
  
  last.time.stamp <- t.list$last.time.stamp
  for(i in 2:(n.list - 1)){
    print(paste0("file matcher index is: ", i))
    
    print(tail(as.vector(my.list[[i + 1]]$time.stamp), 1))
    
    if(last.time.stamp >= tail(as.vector(my.list[[i + 1]]$time.stamp), 1))
      break
    
    t.list <- FileMatcher(my.list[[i]], my.list[[i + 1]], last.time.stamp)
    
    print("ok file matcher")
    
    fin.df <- rbind(fin.df, t.list$my.df)
    print(dim(fin.df))
    last.time.stamp <- t.list$last.time.stamp
    
  }
  
  colnames(fin.df) <- c("symbol1", "time.stamp1", "trading.day1", "close1", "volume1"
                        , "symbol2", "time.stamp2", "trading.day2", "close2", "volume2")
  return(fin.df)
}


FileReader <- function(my.path, my.files){
  
  n <- length(my.files)

  my.list <- NULL
  
  #stop at 3 to test otherwise n
  for(i in 1:n){
    
    print(i)
    my.list[[i]] <- read.csv(paste0(my.path, "/", my.files[i]))
  }
  return(my.list)
}

#format each data frame from the list of all csv files
FileFormater <- function(df){
  
  n.df <- dim(df)[1]
  p.df <- dim(df)[2]
  
  symbol <- as.character(df$symbol)
  close <- df$close
  volume <- df$volume
  
  print(head(df))
  
  print(tail(df))
  
  time.stamp <- df$timestamp
  time.stamp <- gsub("T", " ", substr(time.stamp, 1, 16))
  time.stamp <- as.POSIXct(time.stamp, format = "%Y-%m-%d %H:%M")
  day.of.week <- format(time.stamp, format = "%a")
  
  trading.day <- as.POSIXct(as.character(df$tradingDay), format = "%Y-%m-%d")
  my.df <- data.frame(cbind(symbol, time.stamp, trading.day, close, volume))
  
  print(n.df)
  print(dim(my.df))
    
  return(my.df[1:(n.df - 20), ])
}

FileMatcher <- function(df1, df2, last.time.stamp = 0){
  
  print(dim(df1))
  print(head(df1))
  
  print(tail(df1))
  print(tail(df2))
  
  time.stamp.vec1 <- as.vector(df1$time.stamp)
  time.stamp.vec2 <- as.vector(df2$time.stamp)
  
  if(last.time.stamp > 0){
    
    df1 <- data.frame(df1[time.stamp.vec1 > last.time.stamp, ])
    df2 <- data.frame(df2[time.stamp.vec2 > last.time.stamp, ])
  }

  time.stamp.vec1 <- as.vector(df1$time.stamp)
  time.stamp.vec2 <- as.vector(df2$time.stamp)
  
  symbol1 <- df1$symbol[1]
  symbol2 <- df2$symbol[1]
  
  df1 <- apply(df1[, -1], 2, as.double)
  df2 <- apply(df2[, -1], 2, as.double)
  
  n.df1 <- dim(df1)[1]
  n.df2 <- dim(df2)[1]
  p.df1 <- dim(df1)[2]
  
  print(paste0("n.df1 is: ", n.df1))
  print(paste0("p.df1 is: ", p.df1))
  
  
  my.df <- matrix(data = NA, nrow = n.df1, ncol = (p.df1 * 2))
  
  k <- 1
  
  print(any(is.na(time.stamp.vec1)))
  print(any(is.na(time.stamp.vec2)))

  for(i in 1:n.df1){
    
    while(time.stamp.vec2[k] < time.stamp.vec1[i]){
      
      # print(time.stamp.vec2[k])
      # print(time.stamp.vec1[i])
      
      if(length(time.stamp.vec2[k]) == 0 || is.na(time.stamp.vec2[k + 1])){
        print("time.stamp.vec2[k] is null")
        break
      }

      if(length(time.stamp.vec1[i]) == 0 || is.na(time.stamp.vec1[i])){
        print("time.stamp.vec1[i] is null")
        break
      }
      
      k <- k + 1
    }

    if(time.stamp.vec2[k] == time.stamp.vec1[i]){
      my.df[i, ] <- c(df1[i, ], df2[k, ])
    }
    if(time.stamp.vec2[k] > time.stamp.vec1[i]){
      my.df[i, ] <- c(df1[i, ], df2[(k - 1), ])
    }
  }
  n.df <- dim(my.df)[1]
  symbol1 <- rep(symbol1, n.df)
  symbol2 <- rep(symbol2, n.df)
  
  my.df <- data.frame(cbind(symbol1, data.frame(my.df[, 1:4]), symbol2, data.frame(my.df[, 5:8])))
  return(list(my.df = my.df, last.time.stamp = time.stamp.vec1[n.df1]))
}