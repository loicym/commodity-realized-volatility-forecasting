#for each specifications, same output as HAR models, with list of predictions at 1, 5, and 22 days + forecast errors
HarTvEv <- function(df.rv = list.5.min$rvar
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
  
  df.har.tv.ev <- data.frame(df.rv[, 1])
  
  names(df.har.tv.ev) <- "date"
  temp.date <- df.har.tv.ev
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.sl <- paste("log.term.struct", var.tickers[j], sep = ".")
    name.matu <- paste("time.maturity", var.tickers[j], sep = ".")
    
    dummy.sl <- rep(0, n.df)
    sl <- Lag(df.sl[, name.sl] * 100, start.lag = n.ahead)
    dummy.sl[sl < 0] <- 1
    
    temp.df.har.tv.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                       , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                       , sl
                                       , dummy.sl
                                       , dummy.sl * sl))
    
    
    names(temp.df.har.tv.ev) <- c(paste0("rv.", var.tickers[j])
                               , paste0("har1.", var.tickers[j])
                               , paste0("har5.", var.tickers[j])
                               , paste0("har22.", var.tickers[j])
                               , paste0("dtm.", var.tickers[j])
                               , paste0("sl.", var.tickers[j])
                               , paste0("dum.sl.", var.tickers[j])
                               , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
    
    
    if(any(var.tickers[j] == c("C", "S", "W"))){
      
      temp.df.har.tv.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                         , Lag(mat.calendar.dum[, 8], start.lag = 0)
                                         , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                         , sl
                                         , dummy.sl
                                         , dummy.sl * sl))
      
      
      names(temp.df.har.tv.ev) <- c(paste0("rv.", var.tickers[j])
                                 , paste0("har1.", var.tickers[j])
                                 , paste0("har5.", var.tickers[j])
                                 , paste0("har22.", var.tickers[j])
                                 , paste0(names(mat.calendar.dum)[8], ".", var.tickers[j])
                                 , paste0("dtm.", var.tickers[j])
                                 , paste0("sl.", var.tickers[j])
                                 , paste0("dum.sl.", var.tickers[j])
                                 , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
      
    }
    
    
    if(var.tickers[j] == "NG"){
      
      temp.df.har.tv.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                         , Lag(mat.calendar.dum[, 2], start.lag = 0)
                                         , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                         , sl
                                         , dummy.sl
                                         , dummy.sl * sl))
      
      
      names(temp.df.har.tv.ev) <- c(paste0("rv.", var.tickers[j])
                                 , paste0("har1.", var.tickers[j])
                                 , paste0("har5.", var.tickers[j])
                                 , paste0("har22.", var.tickers[j])
                                 , paste0(names(mat.calendar.dum)[2], ".", var.tickers[j])
                                 , paste0("dtm.", var.tickers[j])
                                 , paste0("sl.", var.tickers[j])
                                 , paste0("dum.sl.", var.tickers[j])
                                 , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
      
    }
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    
    temp.df.har.tv.ev <- cleaner.df(temp.df.har.tv.ev)
    df.har.tv.ev <- cbind(df.har.tv.ev, temp.df.har.tv.ev)
    
    list.formula[[j]] <- formula(paste(names(temp.df.har.tv.ev)[1], paste0(names(temp.df.har.tv.ev)[2:ncol(temp.df.har.tv.ev)], collapse = " + "), sep = " ~ "))     ##more tractable formula:
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    
    # df.har.tv.ev <- df.har.tv.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
    ##BEWARE! the data frame needs to be loaded in the environment
    df.har.tv.ev <- df.har.tv.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
    
    df.har.tv.ev <<- df.har.tv.ev
  }
  
  
  if(out.sample.mode){
    df.har.tv.ev.out <- df.har.tv.ev[(training.sample.length + 1):n.df, ]
    df.har.tv.ev <- df.har.tv.ev[(523:training.sample.length), ]
    
    df.har.tv.ev.out <<- df.har.tv.ev.out
    df.har.tv.ev <<- df.har.tv.ev
  }
  
  #start estimation of the TV-SUR
  mod.har.tv.ev <- tvSURE(formula = list.formula, data = df.har.tv.ev, bw = rep(20, 9), method = "tvFGLS", control = list(tol = 0.1, maxiter = 10))
 
  ##To return the model, uncomment below
  # return(mod.har.tv.ev)
  
  #create a data frame to fill with extracted results:
  res.df.tex <- data.frame(matrix(data = NA, nrow = 20, ncol = 9))
  
  rownames(res.df.tex) <- c("Constant", "1"
                            , "$RV_{c,t-1}$" , "2"
                            , "$RV_{c,t-2|t-5}$", "3"
                            , "$RV_{c,t-6|t-22}$", "4"
                            , "$M_{c,t}$", "5"
                            , "$TM_{c,t} (10^{2})$", "6"
                            , "$SL_{c,t-1} (10^{-2})$", "7"
                            , "$B_{c,t-1}$", "8"
                            , "$SL_{c,t-1} \times B_{c,t-1} (10^{-2})$", "9"
                            , "Bandwidth"
                            , "Pseudo R$^{2}$")
  k <- 1
  

  for(j in 1:9){
    # print(paste("j is : ", j))
    i <- 1
    #get matrix of time varying coeffs
    while(i < 18){
      
      #report coefficients
      res.df.tex[i, j] <- mean(mod.har.tv.ev$coefficients[, k])
      #report standard error of coefficients
      res.df.tex[(i + 1), j] <- sd(mod.har.tv.ev$coefficients[, k], na.rm = T) / length(mod.har.tv.ev$coefficients[, k])^0.5
      #format coefficients 
      res.df.tex[i, j] <- ApplyStars(my.coeff = res.df.tex[i, j], my.se = res.df.tex[(i + 1), j])
      #format standard errors
      res.df.tex[(i + 1), j] <- ApplyBrackets(my.se = res.df.tex[(i + 1), j])
      #switch to new time varying coefficient
      k <- k + 1
      #switch to next two rows in the reporting table
      i <- i + 2
      
      if(all(j != c(1, 2, 3, 6)) && i == 9){
        res.df.tex[i, j] <- "&"
        res.df.tex[(i + 1), j] <- "&"
        i = i + 2
      }
    }
    
    res.df.tex[19, j] <- ApplyDollars(my.val = mod.har.tv.ev$bw[j])
    res.df.tex[20, j] <- ApplyDollars(my.val = 1 - (var(mod.har.tv.ev$residuals[, j]) / var(mod.har.tv.ev$y[, j])))
  }

  
  ##to print model summary, uncomment below
  # print(res.df.tex)

  ##stargazer test (not run)
  # stargazer(res.df.tex, summary = FALSE, digits = 2, digits.extra = 2)
  
  if(out.sample.mode){
    
    df.forecast <- forecast(mod.har.tv.ev, newdata = df.har.tv.ev.out, n.ahead = dim(df.har.tv.ev.out)[1])
    df.real <- df.har.tv.ev.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
  
  if(!out.sample.mode){
    
    df.forecast <- mod.har.tv.ev$fitted
    df.errors <- mod.har.tv.ev$residuals
    df.real <- mod.har.tv.ev$y
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}




RecomputeHarTvEv <- function(mod.tv){
  
  #create a data frame to fill with extracted results:
  res.df.tex <- data.frame(matrix(data = NA, nrow = 29, ncol = 9))
  
  # return(mod.har.tv.ev)
  
  rownames(res.df.tex) <- c("Constant", "1sd", "1se"
                            , "$RV_{c,t-1}$" , "2sd", "2se"
                            , "$RV_{c,t-2|t-5}$", "3sd", "3se"
                            , "$RV_{c,t-6|t-22}$", "4sd", "4se"
                            , "$M_{c,t}$", "5sd", "5se"
                            , "$TM_{c,t} (10^{2})$", "6sd", "6se"
                            , "$SL_{c,t-1} (10^{-2})$", "7sd", "7se"
                            , "$B_{c,t-1}$", "8sd", "8se"
                            , "$SL_{c,t-1} \times B_{c,t-1} (10^{-2})$", "9sd", "9se"
                            , "Bandwidth"
                            , "Pseudo R$^{2}$")
  k <- 1
  
  # return(mod.har.tv.ev)
  # print(dim(coefficients(mod.har.tv.ev)))
  for(j in 1:9){
    # print(paste("j is : ", j))
    i <- 1
    #get matrix of time varying coeffs
    while(i < 27)
    {
      print(i)
      #report coefficients
      res.df.tex[i, j] <- mean(mod.har.tv.ev$coefficients[, k])
      
      #report standard error of coefficients
      # res.df.tex[(i + 1), j] <- sd(mod.har.tv.ev$coefficients[, k], na.rm = T) / length(mod.har.tv.ev$coefficients[, k])^0.5
      
      #format coefficients 
      res.df.tex[i, j] <- ApplyStars(my.coeff = res.df.tex[i, j], my.se = res.df.tex[(i + 1), j])
      
      #format standard errors
      # res.df.tex[(i + 1), j] <- ApplyParenthesis(my.se = res.df.tex[(i + 1), j])
      
      
      res.df.tex
      
      #report min / max
      # res.df.tex[(i + 2), j] <- ApplyMinMax(my.min = min(mod.har.tv.ev$coefficients[, k], na.rm = T), my.max = max(mod.har.tv.ev$coefficients[, k]))
      
      #switch to new time varying coefficient
      k <- k + 1
      #switch to next two rows in the reporting table
      i <- i + 3
      
      if(all(j != c(1, 2, 3, 6)) && i == 13){
        res.df.tex[i, j] <- "&"
        res.df.tex[(i + 1), j] <- "&"
        res.df.tex[(i + 2), j] <- "&"
        i = i + 3
      }
    }
    
    res.df.tex[28, j] <- ApplyDollars(my.val = mod.har.tv.ev$bw[j])
    res.df.tex[29, j] <- ApplyDollars(my.val = 1 - (var(mod.har.tv.ev$residuals[, j]) / var(mod.har.tv.ev$y[, j])))
  }
  
  
  print(res.df.tex)
  
  
  
  
  
}