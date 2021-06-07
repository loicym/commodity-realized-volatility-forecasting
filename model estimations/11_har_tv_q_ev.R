#for each specifications, same output as HAR models, with list of predictions at 1, 5, and 22 days + forecast errors
HarTvQEv <- function(df.rv = list.5.min$rvar
                    , df.sl = list.5.min$slope
                    , df.time.matu = list.5.min$time.matu
                    , mat.calendar.dum = list.5.min$month.dummies
                    , out.sample.mode = FALSE
                    , training.sample.length = 1200
                    , n.ahead = c(1, 5, 22)){
  
  library(texreg)
  library(systemfit)
  library(tvReg)
  library(stargazer)
  
  n.df <- dim(df.rv)[1]
  p.df <- dim(df.rv)[2]
  
  var.tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  list.models <- list()
  list.formula <- list()
  names.y <- names(df.rv)
  
  df.har.tv.q.ev <- data.frame(df.rv[, 1])
  
  names(df.har.tv.q.ev) <- "date"
  temp.date <- df.har.tv.q.ev
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.sl <- paste("log.term.struct", var.tickers[j], sep = ".")
    name.matu <- paste("time.maturity", var.tickers[j], sep = ".")
    name.rq <- paste("realized.log.quarticity.288", var.tickers[j], sep = ".")
    
    dummy.sl <- rep(0, n.df)
    sl <- Lag(df.sl[, name.sl] * 100, start.lag = n.ahead)
    dummy.sl[sl < 0] <- 1
    
    temp.df.har.tv.q.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                          , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                          , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1) * Lag(0.5 * LagMean(df.rv[, name.rq], start.lag = 1, end.lag = 1), start.lag = n.ahead - 1)#day lag
                                          , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                          , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                          , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                          , sl
                                          , dummy.sl
                                          , dummy.sl * sl))
    
    
    names(temp.df.har.tv.q.ev) <- c(paste0("rv.", var.tickers[j])
                                  , paste0("har1.", var.tickers[j])
                                  , paste0("har.rq.1", var.tickers[j])
                                  , paste0("har5.", var.tickers[j])
                                  , paste0("har22.", var.tickers[j])
                                  , paste0("dtm.", var.tickers[j])
                                  , paste0("sl.", var.tickers[j])
                                  , paste0("dum.sl.", var.tickers[j])
                                  , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)

    
    if(any(var.tickers[j] == c("C", "S", "W"))){
      
      temp.df.har.tv.q.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                           #dependent variable
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1) * Lag(0.5 * LagMean(df.rv[, name.rq], start.lag = 1, end.lag = 1), start.lag = n.ahead - 1)#day lag
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)           #week lag
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                           , Lag(mat.calendar.dum[, 8], start.lag = 0)
                                           , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                           , sl
                                           , dummy.sl
                                           , dummy.sl * sl))
      
      
      names(temp.df.har.tv.q.ev) <- c(paste0("rv.", var.tickers[j])
                                   , paste0("har1.", var.tickers[j])
                                   , paste0("har.rq.1", var.tickers[j])
                                   , paste0("har5.", var.tickers[j])
                                   , paste0("har22.", var.tickers[j])  
                                   , paste0(names(mat.calendar.dum)[8], ".", var.tickers[j])
                                   , paste0("dtm.", var.tickers[j])
                                   , paste0("sl.", var.tickers[j])
                                   , paste0("dum.sl.", var.tickers[j])
                                   , paste0("dum.sl.interact.", var.tickers[j]))
    }
    
    
    if(var.tickers[j] == "NG"){
      
      temp.df.har.tv.q.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                           #dependent variable
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1) * Lag(0.5 * LagMean(df.rv[, name.rq], start.lag = 1, end.lag = 1), start.lag = n.ahead - 1)#day lag
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)           #week lag
                                           , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                           , Lag(mat.calendar.dum[, 2], start.lag = 0)
                                           , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                           , sl
                                           , dummy.sl
                                           , dummy.sl * sl))
      
      
      names(temp.df.har.tv.q.ev) <- c(paste0("rv.", var.tickers[j])
                                   , paste0("har1.", var.tickers[j])
                                   , paste0("har.rq.1", var.tickers[j])
                                   , paste0("har5.", var.tickers[j])
                                   , paste0("har22.", var.tickers[j])  
                                   , paste0(names(mat.calendar.dum)[2], ".", var.tickers[j])
                                   , paste0("dtm.", var.tickers[j])
                                   , paste0("sl.", var.tickers[j])
                                   , paste0("dum.sl.", var.tickers[j])
                                   , paste0("dum.sl.interact.", var.tickers[j]))
    }
    
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    temp.df.har.tv.q.ev <- cleaner.df(temp.df.har.tv.q.ev)
    df.har.tv.q.ev <- cbind(df.har.tv.q.ev, temp.df.har.tv.q.ev)
    list.formula[[j]] <- formula(paste(names(temp.df.har.tv.q.ev)[1], paste0(names(temp.df.har.tv.q.ev)[2:ncol(temp.df.har.tv.q.ev)], collapse = " + "), sep = " ~ "))     ##more tractable formula:
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    
    # df.har.tv.q.ev <- df.har.tv.q.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
    ##BEWARE! the data frame needs to be loaded in the environment
    df.har.tv.q.ev <- df.har.tv.q.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
    df.har.tv.q.ev <<- df.har.tv.q.ev
  }
  
  if(out.sample.mode){
    
    df.har.tv.q.ev.out <- df.har.tv.q.ev[(training.sample.length + 1):n.df, ]
    df.har.tv.q.ev <- df.har.tv.q.ev[(523:training.sample.length), ]
    
    df.har.tv.q.ev.out <<- df.har.tv.q.ev.out
    df.har.tv.q.ev <<- df.har.tv.q.ev
    
  }
  
  #start estimation of the TV-SUR
  mod.har.tv.q.ev <- tvSURE(formula = list.formula, data = df.har.tv.q.ev, bw = rep(20, 9), method = "tvFGLS", control = list(tol = 0.1, maxiter = 10))
  
  ##to return the model, uncomment below
  # return(mod.har.tv.q.ev)
  
  #create a data frame to fill with extracted results:
  res.df.tex <- data.frame(matrix(data = NA, nrow = 32, ncol = 9))
  
  
  rownames(res.df.tex) <- c("Constant", "se1", "min/max1"
                            , "$RV_{c,t-1}$" , "se2", "min/max2"
                            , "$RV_{c,t-1} \times RQ_{c,t-1}$", "se3", "min/max3"
                            , "$RV_{c,t-2|t-5}$", "se3", "min/max3"
                            , "$RV_{c,t-6|t-22}$", "se4", "min/max4"
                            , "$M_{c,t}$", "se5", "min/max5"
                            , "$TM_{c,t} (10^{2})$", "se6", "min/max6"
                            , "$SL_{c,t-1} (10^{-2})$", "se7", "min/max7"
                            , "$B_{c,t-1}$", "se8", "min/max8"
                            , "$SL_{c,t-1} \times B_{c,t-1} (10^{-2})$", "se9", "min/max9"
                            , "Bandwidth"
                            , "Pseudo R$^{2}$")
  
  k <- 1
  
  for(j in 1:9){
    i <- 1
    #get matrix of time varying coeffs
    while(i < 30){
      
      #report coefficients
      res.df.tex[i, j] <- mean(mod.har.tv.q.ev$coefficients[, k])
      #report standard error of coefficients
      res.df.tex[(i + 1), j] <- sd(mod.har.tv.q.ev$coefficients[, k], na.rm = T) / length(mod.har.tv.q.ev$coefficients[, k])^0.5
      #format coefficients 
      res.df.tex[i, j] <- ApplyStars(my.coeff = res.df.tex[i, j], my.se = res.df.tex[(i + 1), j])
      #format standard errors
      res.df.tex[(i + 1), j] <- ApplyParenthesis(my.se = res.df.tex[(i + 1), j])
      #report min / max
      res.df.tex[(i + 2), j] <- ApplyMinMax(my.min = min(mod.har.tv.q.ev$coefficients[, k], na.rm = T), my.max = max(mod.har.tv.q.ev$coefficients[, k], na.rm = T))
      #switch to new time varying coefficient
      k <- k + 1
      #switch to next two rows in the reporting table
      i <- i + 3
      
      if(all(j != c(1, 2, 3, 6)) && i == 16){
        
        res.df.tex[i, j] <- "&"
        res.df.tex[(i + 1), j] <- "&"
        res.df.tex[(i + 2), j] <- "&"
        
        i = i + 3
      }
    }
    
    res.df.tex[21, j] <- ApplyDollars(my.val = mod.har.tv.q.ev$bw[j])
    res.df.tex[22, j] <- ApplyDollars(my.val = 1 - (var(mod.har.tv.q.ev$residuals[, j]) / var(mod.har.tv.q.ev$y[, j])))
  }
  
  ##print the model summary
  # print(res.df.tex)
  
  ##export model summary with stargazer
  # stargazer(res.df.tex, summary = FALSE, digits = 2, digits.extra = 2)
  
  
  if(out.sample.mode){
    
    df.forecast <- forecast(mod.har.tv.q.ev, newdata = df.har.tv.q.ev.out, n.ahead = dim(df.har.tv.q.ev.out)[1])
    df.real <- df.har.tv.q.ev.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
  
  if(!out.sample.mode){
    
    df.forecast <- mod.har.tv.q.ev$fitted
    df.errors <- mod.har.tv.q.ev$residuals
    df.real <- mod.har.tv.q.ev$y
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}


##Function using an already estimated TV-SUR model object to reprint summary
RecomputeHarTvQEv <- function(mod.har.tv.q.ev){
  
  
  #create a data frame to fill with extracted results:
  res.df.tex <- data.frame(matrix(data = NA, nrow = 32, ncol = 9))
  
  
  rownames(res.df.tex) <- c("Constant", "se1", "min_max1"
                            , "$RV_{c,t-1}$" , "se2", "min_max2"
                            , "$RV_{c,t-1} \times RQ_{c,t-1}$", "se3", "min_max3"
                            , "$RV_{c,t-2|t-5}$", "se4", "min_max4"
                            , "$RV_{c,t-6|t-22}$", "se5", "min_max5"
                            , "$M_{c,t}$", "se6", "min_max6"
                            , "$TM_{c,t} (10^{2})$", "se7", "min_max7"
                            , "$SL_{c,t-1} (10^{-2})$", "se8", "min_max8"
                            , "$B_{c,t-1}$", "se9", "min_max9"
                            , "$SL_{c,t-1} \times B_{c,t-1} (10^{-2})$", "se10", "min_max10"
                            , "Bandwidth"
                            , "Pseudo R$^{2}$")
  
  k <- 1
  
  for(j in 1:9){
    i <- 1
    #get matrix of time varying coeffs
    while(i < 30){
      
      #report coefficients
      res.df.tex[i, j] <- mean(mod.har.tv.q.ev$coefficients[, k])
      #report standard error of coefficients
      res.df.tex[(i + 1), j] <- sd(mod.har.tv.q.ev$coefficients[, k], na.rm = T) / length(mod.har.tv.q.ev$coefficients[, k])^0.5
      #format coefficients 
      res.df.tex[i, j] <- ApplyStars(my.coeff = res.df.tex[i, j], my.se = res.df.tex[(i + 1), j])
      #format standard errors
      res.df.tex[(i + 1), j] <- ApplyParenthesis(my.se = res.df.tex[(i + 1), j])
      #report min / max
      res.df.tex[(i + 2), j] <- ApplyMinMax(my.min = min(mod.har.tv.q.ev$coefficients[, k], na.rm = T), my.max = max(mod.har.tv.q.ev$coefficients[, k], na.rm = T))
      #switch to new time varying coefficient
      k <- k + 1
      #switch to next two rows in the reporting table
      i <- i + 3
      
      if(all(j != c(1, 2, 3, 6)) && i == 16){
        res.df.tex[i, j] <- "&"
        res.df.tex[(i + 1), j] <- "&"
        res.df.tex[(i + 2), j] <- "&"
        
        i = i + 3
      }
    }
    
    res.df.tex[21, j] <- ApplyDollars(my.val = mod.har.tv.q.ev$bw[j])
    res.df.tex[22, j] <- ApplyDollars(my.val = 1 - (var(mod.har.tv.q.ev$residuals[, j]) / var(mod.har.tv.q.ev$y[, j])))
  }
  
  ##print model summary
  print(res.df.tex)
}