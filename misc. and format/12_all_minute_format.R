AllMinuteFormat <- function(mat.min, my.freq = c(1440, 288, 96, 24, 1)){
  library(stargazer)
  library(fracdiff)
  library(sandwich)
  library(lmtest)
  
  n.mat <- dim(mat.min)[1]
  p.mat <- dim(mat.min)[2]
  
  multiplicator <- c(50, 50, 50, 1000, 42000, 10000, 100, 25000, 5000)
  
  names.mat <- colnames(mat.min)
  
  my.date <- mat.min[, "date"]
  close1 <- mat.min[, substring(names.mat, 1, 6) == "close1"]
  ret1 <- rbind(rep(0, 9), close1[2:n.mat, ] / close1[1:(n.mat - 1), ] - 1)
  log.ret1 <- rbind(rep(0, 9), log(close1[2:n.mat, ] / close1[1:(n.mat - 1), ]))
  volume1 <- mat.min[, substring(names.mat, 1, 7) == "volume1"]
  turnover1 <- sweep(volume1 * close1, 2, multiplicator, FUN = "*")
  
  print(any(is.na(log.ret1)))
  print(head(ret1))
  
  print(dim(volume1))
  print(dim(close1))
  print(dim(turnover1))
  
  close1.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  log.ret1.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  bid.ask.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  turnover.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  volume.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  count.trade.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  rvar.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  jump.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  #tripower quarticity (Barndorff-Nielsen and Shephard 2006)
  tq.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  #realized quarticity (Barndorff-Nielsen and Shephard 2002)
  rq.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  
  #log realized quarticity (Barndorff-Nielsen and Shephard 2002) used in Bucchieri and Corsi (2019) modification for logs
  rq.log.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  #bipower variation
  bv.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  #huang and tauchen (2005) z statistic for microstructure noise
  z.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  signif.jump.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  signif.continuous.daily <- matrix(data = NA, nrow = n.mat, ncol = 9)
  
  delta.star.min <- rep(0, 9)
  
  mu1 <- (2 / pi) ^ 0.5
  
  fin.date <- rep(NA, n.mat)
  
  for(j in 1:9){
    k <- 1
    for(i in 1:(n.mat - 1)){
      
      if(my.date[i] != my.date[i + 1] && i > k + 1){
        
        fin.date[i] <- my.date[i]
        
        close1.daily[i, j] <- close1[i, j]
        log.ret1.daily[i, j] <- log(close1[i, j] / close1[k, j])
        turnover.daily[i, j] <- sum(turnover1[k:i, j])
        volume.daily[i, j] <- sum(volume1[k:i, j])
        count.trade.daily[i, j] <- sum(volume1[k:i, j]!=0)
        bid.ask.daily[i, j] <- 2 * max(0, - cov(ret1[(k + 1):i, j], ret1[k:(i - 1), j]))^0.5
        rvar.daily[i, j] <- sum(log.ret1[k:i, j]^2)
        jump.daily[i, j] <- max(0, rvar.daily[i, j] - mu1^(-2) * sum(abs(log.ret1[(k + 1):i, j]) * abs(log.ret1[k:(i - 1), j])))
        
        bv.daily[i, j] <- ((2/pi) ^ 0.5)^-2 * sum(abs(log.ret1[(k + 1):i, j]) * abs(log.ret1[k:(i - 1), j]))
        
        #tq.daily as in Barndorff-Nielsen (2006), used to compute jumps
        tq.daily[i, j] <- (my.freq * ((2 ^ (2 / 3)) * gamma(7 / 6) * (1 / gamma(0.5)))^-3) * sum(abs(log.ret1[(k + 2):i, j])^(4 / 3) * abs(log.ret1[(k + 1):(i - 1), j])^(4/3) * abs(log.ret1[k:(i - 2), j])^(4/3))
        
        #rq.daily as in Barndorff-Nielsen (2002), used in Buccheri and Corsi (2019)
        rq.daily[i, j] <- 2 * (my.freq / 3) * sum(log.ret1[k:i, j]^4)
        
        #log realized quarticity (Barndorff-Nielsen and Shephard 2002) used in Bucchieri and Coris (2019) modification for logs 
        
        if(rvar.daily[i, j] != 0){
          rq.log.daily[i, j] <- (2 / 3) * (sum(log.ret1[k:i, j]^4) / (rvar.daily[i, j])^2)
        }
        if(rvar.daily[i, j] <= 0){
          rq.log.daily[i, j] <- 0
        }
        
        z.daily[i, j] <- my.freq^0.5 * ( (jump.daily[i, j] * rvar.daily[i, j]^(-1)) / ( ( (mu1^(-4) + 2 * mu1^(-2) - 5) * max(1, tq.daily[i, j] * bv.daily[i, j]^(-2)) )^0.5 ) )
        
        #if rvar.daily is 0, then inverse is Inf and z.daily is nan.
        if(is.na(z.daily[i, j]))
          z.daily[i, j] <- 0
        
        
        #use threshold of Andersen Bollerslev Diebold (2007): alpha = 0.999 or phi = 3.090 (level of normal distribution)
        if(z.daily[i, j] > 3.09){
          signif.jump.daily[i, j] <- jump.daily[i, j]
          signif.continuous.daily[i, j] <- bv.daily[i, j]
        }
        
        if(z.daily[i, j] <= 3.09){
          signif.jump.daily[i, j] <- 0
          signif.continuous.daily[i, j] <- rvar.daily[i, j]
        }
        
        k <- i + 1
      }
    }
  }
  
  
  print(sum(is.na(turnover.daily)))
  print(sum(is.na(bid.ask.daily)))
  print(sum(is.na(volume.daily)))
  
  fin.date <- na.omit(fin.date)
  print(length(fin.date))
  
  close1.daily <- FormatDataFrame(close1.daily, df.name = paste0("close1.", as.character(my.freq)))
  log.ret1.daily <- FormatDataFrame(log.ret1.daily, df.name = paste0("log.ret1.", as.character(my.freq)))
  bid.ask.daily <- FormatDataFrame(bid.ask.daily, df.name = paste0("bid.ask.", as.character(my.freq)))
  turnover.daily <- FormatDataFrame(turnover.daily, df.name = paste0("turnover.", as.character(my.freq)))
  volume.daily <- FormatDataFrame(volume.daily, df.name = paste0("volume.", as.character(my.freq)))
  count.trade.daily <- FormatDataFrame(count.trade.daily, df.name = paste0("count.trade.", as.character(my.freq)))
  rvar.daily <- FormatDataFrame(rvar.daily, df.name = paste0("rvar.", as.character(my.freq)))
  jump.daily <- FormatDataFrame(jump.daily, df.name = paste0("jump.", as.character(my.freq)))
  signif.jump.daily <- FormatDataFrame(signif.jump.daily, df.name = paste0("signif.jump.", as.character(my.freq)))
  signif.continuous.daily <- FormatDataFrame(signif.continuous.daily, df.name = paste0("signif.continuous.", as.character(my.freq)))
  bv.daily <- FormatDataFrame(bv.daily, df.name = paste0("bipower.variation.", as.character(my.freq)))
  rq.daily <- FormatDataFrame(rq.daily, df.name = paste0("realized.quarticity.", as.character(my.freq)))
  rq.log.daily <- FormatDataFrame(rq.log.daily, df.name = paste0("realized.log.quarticity.", as.character(my.freq)))
  
  # return(list(bid.ask.daily = bid.ask.daily, log.ret1.daily = log.ret1.daily))
  
  for(j in 1:9){
    delta.star.min[j] <- OptimalSampling19(t.year = dim(bid.ask.daily)[1] / 252, sigma = sd(log.ret1.daily[, j]) * 252^0.5, a = mean(bid.ask.daily[, j]) / 2)  
  }
  
  print(delta.star.min)
  
  
  print(length(fin.date))
  print(dim(rq.daily))
  print(dim(rq.log.daily))
  ##return data frame of all information
  return(data.frame(cbind(fin.date
                          , close1.daily
                          , bid.ask.daily
                          , turnover.daily
                          , volume.daily
                          , count.trade.daily
                          , rvar.daily
                          , jump.daily
                          , signif.jump.daily
                          , signif.continuous.daily
                          , bv.daily
                          , rq.daily
                          , rq.log.daily)))
}


FormatDataFrame <- function(df, df.name){
  
  df <- na.omit(df)
  # df <- data.frame(cbind(fin.date, df))
  colnames(df) <- paste0(df.name, c(".C", ".S", ".W", ".CL", ".HO", ".NG", ".GC", ".HG", ".SI"))
  
  print(dim(df))
  return(df)
}

ListFromDataFrames <- function(df.min){
  
  date <- df.min$fin.date
  df.min <- df.min[, -1]
  
  # list.min <- list()
  p.df <- dim(df.min)[2]
  
  list.min <- list(close1 = data.frame(cbind(date, df.min[, 1:9]))
                   , bid.ask = data.frame(cbind(date, df.min[, 10:18]))
                   , turnover = data.frame(cbind(date, df.min[, 19:27]))
                   , volume = data.frame(cbind(date, df.min[, 28:36]))
                   , count.trade = data.frame(cbind(date, df.min[, 37:45]))
                   , rvar = data.frame(cbind(date, df.min[, 46:54]))
                   , jump = data.frame(cbind(date, df.min[, 55:63]))
                   , signif.jump = data.frame(cbind(date, df.min[, 64:72]))
                   , signif.continuous = data.frame(cbind(date, df.min[, 73:81]))
                   , bipower.variation = data.frame(cbind(date, df.min[, 82:90]))
                   , realized.quarticity = data.frame(cbind(date, df.min[, 91:99]))
                   , realized.log.quarticity = data.frame(cbind(date, df.min[, 100:108])))
  
  return(list.min)
}