#in sample estimation
HarEv <- function(df.rv = list.5.min$rvar
                  , df.sl = list.5.min$slope
                  , df.time.matu = list.5.min$time.matu
                  , mat.calendar.dum = list.5.min$month.dummies
                  , out.sample.mode = FALSE
                  , training.sample.length = 1200
                  , n.ahead = c(1, 5, 22)){
  
  library(texreg)
  library(systemfit)
  
  n.df <- dim(df.rv)[1]
  p.df <- dim(df.rv)[2]
  
  var.tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  list.models <- list()
  list.formula <- list()
  names.y <- names(df.rv)
  
  df.har.ev <- data.frame(df.rv[, 1])

  names(df.har.ev) <- "date"
  temp.date <- df.har.ev
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.sl <- paste("log.term.struct", var.tickers[j], sep = ".")
    name.matu <- paste("time.maturity", var.tickers[j], sep = ".")
    
    dummy.sl <- rep(0, n.df)
    sl <- Lag(df.sl[, name.sl] * 100, start.lag = n.ahead)
    dummy.sl[sl < 0] <- 1
    
    temp.df.har.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                       , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                       , sl
                                       , dummy.sl
                                       , dummy.sl * sl))
    
    
    names(temp.df.har.ev) <- c(paste0("rv.", var.tickers[j])
                               , paste0("har1.", var.tickers[j])
                               , paste0("har5.", var.tickers[j])
                               , paste0("har22.", var.tickers[j])
                               , paste0("dtm.", var.tickers[j])
                               , paste0("sl.", var.tickers[j])
                               , paste0("dum.sl.", var.tickers[j])
                               , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
    
    
    
    if(any(var.tickers[j] == c("C", "S", "W"))){
    
      temp.df.har.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                         , Lag(mat.calendar.dum[, 8], start.lag = 0)
                                         , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                         , sl
                                         , dummy.sl
                                         , dummy.sl * sl))
      
      
      names(temp.df.har.ev) <- c(paste0("rv.", var.tickers[j])
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
      
      temp.df.har.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)          #day lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)          #week lag
                                         , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)
                                         , Lag(mat.calendar.dum[, 2], start.lag = 0)
                                         , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, 0)
                                         , sl
                                         , dummy.sl
                                         , dummy.sl * sl))
      
      
      names(temp.df.har.ev) <- c(paste0("rv.", var.tickers[j])
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
    
    temp.df.har.ev <- cleaner.df(temp.df.har.ev)
    df.har.ev <- cbind(df.har.ev, temp.df.har.ev)
    
    list.formula[[j]] <- formula(paste(paste(names(temp.df.har.ev)[1], paste0(names(temp.df.har.ev)[2:ncol(temp.df.har.ev)], collapse = " + "), sep = " ~ ")))     ##more tractable formula:
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    df.har.ev <- df.har.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
  }
  
  if(out.sample.mode){
    df.har.ev.out <- df.har.ev[(training.sample.length + 1):n.df, ]
    df.har.ev <- df.har.ev[(1:training.sample.length), ]
  }
  
  #start estimation of the SUR
  mod.har.ev <- systemfit(list.formula, data = df.har.ev, method = "SUR")
  
  ##to return the model, unselect below
  # return(mod.har.ev)
  
  
  t.extract <- extract(mod.har.ev, beside = TRUE, include.rsquared = FALSE)

  
  for(j in 1:9){

    t.extract[[j]]@coef.names <- c("Constant"
                                   , "$RV_{c,t-1}$"
                                   , "$RV_{c,t-2|t-5}$"
                                   , "$RV_{c,t-6|t-22}$"
                                   , "$TM_{c,t} (10^{2})$"
                                   , "$SL_{c,t-1} (10^{-2})$"
                                   , "$B_{c,t-1}$"
                                   , "$SL_{c,t-1} * B_{c,t-1} (10^{-2})$")
    
    
    if(any(var.tickers[j] == c("C", "S", "W", "NG"))){
      
      t.extract[[j]]@coef.names <- c("Constant"
                                     , "$RV_{c,t-1}$"
                                     , "$RV_{c,t-2|t-5}$"
                                     , "$RV_{c,t-6|t-22}$"
                                     , "$M_{c,t}$"
                                     , "$TM_{c,t} (10^{2})$"
                                     , "$SL_{c,t-1} (10^{-2})$"
                                     , "$B_{c,t-1}$"
                                     , "$SL_{c,t-1} * B_{c,t-1} (10^{-2})$")
      
    }
  }
  
  ##to print the model summary:
  print(texreg(t.extract
               , single.row = FALSE
               #, custom.header = list("$RV_{c, t+1}$" = 1:9)
               , custom.model.names = var.tickers
               , stars = c(0.01, 0.05, 0.1)))
  
  
  print(paste("ols r.squared", summary(mod.har.ev)$ols.r.squared))
  print(paste("mcelroy r.squared", summary(mod.har.ev)$mcelroy.r.squared))
  print(paste("log likelihood", logLik(mod.har.ev)))
  
  if(out.sample.mode){
    
    df.forecast <- predict(mod.har.ev, newdata = df.har.ev.out)
    df.real <- df.har.ev.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
    
  }
  
  if(!out.sample.mode){
    
    df.forecast <- predict(mod.har.ev)
    df.errors <- residuals(mod.har.ev)
    df.real <- df.forecast + df.errors
    
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}