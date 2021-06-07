ChangeFrequency <- function(df, my.freq){
  
  n.df <- dim(df)[1]
  p.df <- dim(df)[2]
  
  names.df <- colnames(df)
  
  fin.df <- matrix(data = NA, nrow = n.df, ncol = p.df) 
  time.stamp <- df[, "time.stamp"]
  
  print(typeof(time.stamp))
  date <- df[, "date"]
  
  k <- 1
  l <- 1
  for(i in 1:(n.df - 1)){
    if(i%%100000 == 0)
      print(i)
    
    if(date[i] != date[i + 1]){

      ###mode daily stats:
      if(k < i){
        fin.df[l, substring(names.df, 1, 4) == "volu"] <- colSums(df[k:i, substring(names.df, 1, 4) == "volu"])
        fin.df[l, substring(names.df, 1, 4) != "volu"] <- df[i, substring(names.df, 1, 4) != "volu"]
        l <- l + 1
      }

      ###end of mode daily stats
      
      k <- i + 1
    }
    
    ### mode intraday stats
    # if(time.stamp[i] == time.stamp[k] + my.freq * 60){
    #   
    #   fin.df[l, substring(names.df, 1, 4) == "volu"] <- colSums(df[k:i, substring(names.df, 1, 4) == "volu"])
    #   fin.df[l, substring(names.df, 1, 4) != "volu"] <- df[i, substring(names.df, 1, 4) != "volu"]
    #   l <- l + 1
    #   k <- i
    # }
    # 
    # if(time.stamp[i] > time.stamp[k] + my.freq * 60){
    #   
    #   if(k < (i - 1))
    #     fin.df[l, substring(names.df, 1, 4) == "volu"] <- colSums(df[k:(i - 1), substring(names.df, 1, 4) == "volu"])
    #   
    #   if(k == (i - 1))
    #     fin.df[l, substring(names.df, 1, 4) == "volu"] <- df[i - 1, substring(names.df, 1, 4) == "volu"]
    #   
    #   fin.df[l, substring(names.df, 1, 4) != "volu"] <- df[(i - 1), substring(names.df, 1, 4) != "volu"]
    #   l <- l + 1
    #   k <- i
    # }
  }
  
  fin.df <- na.omit(fin.df)
  colnames(fin.df) <- names.df
  return(fin.df)
}



#function to make daily data, match by date all daily contracts
MakeDaily <- function(df){
  
  n.x <- dim(df)[1]
  p.X <- dim(df)[2]
  
  my.date <- df$my.date
  
  fin.df <- data.frame(matrix(data = NA, nrow = 1, ncol = p.X + 1))
  colnames(fin.df) <- paste0(c(colnames(df), "nb.obs"), my.append)
  
  print(fin.df)
  # return()
  k <- 1
  for(i in 1:(n.x - 1)){
    
    if(my.date[i + 1] != my.date[i]){
      
      #create a tempDataframe
      t.df <- as.vector(c(df$my.date[i]
              , df$symbol1[i]
              , df$time.stamp1[i]
              , df$trading.day1[i]
              , df$close1[i]
              , sum(df$volume1[k:i])
              , df$symbol2[i]
              , df$time.stamp2[i]
              , df$trading.day2[i]
              , df$close2[i]
              , sum(df$volume2[k:i])
              , df$time.maturity[i]
              , df$maturity.gap[i]
              , df$log.term.struct[i]
              , sum(df$ret1[k:i])
              , sum(df$ret2[k:i])
              , sum(df$rv1[k:i])
              , sum(df$rv2[k:i])
              , sum(df$cov[k:i])
              , sum(df$slrv[k:i])
              , (i - k + 1)))
              
        k = (i + 1)
        
        if(i %% 100 == 0)
          print(i)
        fin.df <- rbind(fin.df, t.df)
      }
  }
  fin.df <- data.frame(fin.df[-c(1, 2), ])
  return(fin.df)
}




#function to make daily data, match by date all daily contracts
MakeDailyFactor <- function(df){
  
  n.x <- dim(df)[1]
  p.X <- dim(df)[2]
  
  my.date <- df$my.date
  
  fin.df <- data.frame(matrix(data = NA, nrow = 1, ncol = 34))
  
  # df <- apply(df, 2, as.double)
  
  print(fin.df)
  k <- 1
  
  for(i in 1:(n.x - 1)){
    
    if(my.date[i + 1] != my.date[i]){
      
      # print(length(log(rowMeans(df[k:i, c(4, 5, 6)])+1)^2))
      # return()
      
      # print(sum(log(df[k:i, 12] + 1)^2))
      # print(sum(df[k:i, 21])^0.5)
      
      #create a tempDataframe
      t.df <- as.vector(c(df$my.date[i]
                          , df$time.stamp[i]
                          , df$trading.day[i]
                          , sum(log(df[k:i, 4] + 1)^2)                                #indiv commodities rv for verifications
                          , sum(log(df[k:i, 5] + 1)^2)
                          , sum(log(df[k:i, 6] + 1)^2)
                          , sum(log(df[k:i, 7] + 1)^2)
                          , sum(log(df[k:i, 8] + 1)^2)
                          , sum(log(df[k:i, 9] + 1)^2)
                          , sum(log(df[k:i, 10] + 1)^2)
                          , sum(log(df[k:i, 11] + 1)^2)
                          , sum(log(df[k:i, 12] + 1)^2)
                          , sum(log(rowMeans(df[k:i, c(4, 5, 6)]) + 1)^2)             #start the full factors combinations
                          , sum(log(rowMeans(df[k:i, c(7, 8, 9)]) + 1)^2)
                          , sum(log(rowMeans(df[k:i, c(10, 11, 12)]) + 1)^2)
                          , sum(log(rowMeans(df[k:i, 4:12]) + 1)^2)                   #start the full market factor
                          , sum(log(rowMeans(df[k:i, c(5, 6)]) + 1)^2)                #start all nine factors with no commodity overlap:#corn
                          , sum(log(rowMeans(df[k:i, c(4, 6)]) + 1)^2)                #soybeans
                          , sum(log(rowMeans(df[k:i, c(4, 5)]) + 1)^2)                #wheat
                          , sum(log(rowMeans(df[k:i, c(8, 9)]) + 1)^2)                #crude oil
                          , sum(log(rowMeans(df[k:i, c(7, 9)]) + 1)^2)                #heating oil
                          , sum(log(rowMeans(df[k:i, c(7, 8)]) + 1)^2)                #natural gas
                          , sum(log(rowMeans(df[k:i, c(11, 12)]) + 1)^2)              #gold
                          , sum(log(rowMeans(df[k:i, c(10, 12)]) + 1)^2)              #copper
                          , sum(log(rowMeans(df[k:i, c(10, 11)]) + 1)^2)              #silverhead
                          
                          , max(c(0, (sum(log(df[k:i, 4] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 13])))  #start estimation of individual jumps as in Andersen et al. 2007 (estimate the squared root jump!!)
                          , max(c(0, (sum(log(df[k:i, 5] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 14])))
                          , max(c(0, (sum(log(df[k:i, 6] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 15])))
                          , max(c(0, (sum(log(df[k:i, 7] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 16])))
                          , max(c(0, (sum(log(df[k:i, 8] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 17])))
                          , max(c(0, (sum(log(df[k:i, 9] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 18])))
                          , max(c(0, (sum(log(df[k:i, 10] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 19])))
                          , max(c(0, (sum(log(df[k:i, 11] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 20])))
                          , max(c(0, (sum(log(df[k:i, 12] + 1)^2) - ( (2/pi) ^ 0.5) )^-2 * sum(df[k:i, 21])))
                          ))
      
      k <- i + 1
      
      # print(t.df)
      
      fin.df <- rbind(fin.df, t.df)
    }
    if(i %% 50000 == 0){
      print(i)
    }
  }
  fin.df <- data.frame(fin.df)
  
  names(fin.df) <- c("my.date"
                     , "time.stamp"
                     , "trading.day"
                     , "C"
                     , "S"
                     , "W"
                     , "CL"
                     , "HO"
                     , "NG"
                     , "GC"
                     , "HG"
                     , "SI"
                     , "AGRI"
                     , "ENERGY"
                     , "METAL"
                     , "MKT"
                     , "AgriC"
                     , "AgriS"
                     , "AgriW"
                     , "EnerCL"
                     , "EnerHO"
                     , "EnerNG"
                     , "MetalGC"
                     , "MetalHG"
                     , "MetalSI"
                     , "j.c"
                     , "j.s"
                     , "j.w"
                     , "j.cl"
                     , "j.ho"
                     , "j.ng"
                     , "j.gc"
                     , "j.hg"
                     , "j.si")
  return(fin.df)
}



MatchAll <- function(df1, df2, df3, df4, df5, df6, df7, df8, df9){
  
  base.date <- df4$my.date.CL
  
  m1 <- match(base.date, df1$my.date.C)
  m2 <- match(base.date, df2$my.date.S)
  m3 <- match(base.date, df3$my.date.W)
  m4 <- match(base.date, df4$my.date.CL)
  m5 <- match(base.date, df5$my.date.HO)
  m6 <- match(base.date, df6$my.date.NG)
  m7 <- match(base.date, df7$my.date.GC)
  m8 <- match(base.date, df8$my.date.HG)
  m9 <- match(base.date, df9$my.date.SI)
  
  print(m1)
  print(m2)
  
  
  df1 <- df1[m1, ]
  df2 <- df2[m2, ]
  df3 <- df3[m3, ]
  df4 <- df4[m4, ]
  df5 <- df5[m5, ]
  df6 <- df6[m6, ]
  df7 <- df7[m7, ]
  df8 <- df8[m8, ]
  df9 <- df9[m9, ]
  
  ##specific selection
  # df1 <- data.frame(cbind(df1$my.date.C, df1$log.term.struct.C))
  # df2 <- data.frame(df2$log.term.struct.S)
  # df3 <- data.frame(df3$log.term.struct.W)
  # df4 <- data.frame(df4$log.term.struct.CL)
  # df5 <- data.frame(df5$log.term.struct.HO)
  # df6 <- data.frame(df6$log.term.struct.NG)
  # df7 <- data.frame(df7$log.term.struct.GC)
  # df8 <- data.frame(df8$log.term.struct.HG)
  # df9 <- data.frame(df9$log.term.struct.SI)
  
  fin.df <- data.frame(cbind(df1, df2, df3, df4, df5, df6, df7, df8, df9))
  return(fin.df)
}


CorrectTimeMatu <- function(df){
  
  # getwd("C:/Users/loicy/Dropbox/unine/loic/thesis/R/RProj2 - rv/raw data/1 min/make_daily")
  n.df <- dim(df)[1]
  df.names <- names(df)
  
  ind.mat <- which(substring(df.names, 1, 8) == "time.mat")
  ind.symbol <- which(substring(df.names, 1, 7) == "symbol1")
  
  df[, ind.mat] <- NA
  ref.point <- 0
  
  for(i in n.df:2){
    
    if(df[i, ind.symbol] != df[(i - 1), ind.symbol]){
      df[(i - 1), ind.mat] <- 0
      ref.point <- df[(i - 1), 1]
    }
    
    if(df[i, ind.symbol] == df[(i - 1), ind.symbol] && ref.point != 0){
      df[(i - 1), ind.mat] <- ref.point - df[(i - 1), 1]
    }
  }
  
  return(df)
}


