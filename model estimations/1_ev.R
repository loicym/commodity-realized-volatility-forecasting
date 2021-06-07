#in sample estimation
Ev <- function(df.rv = list.5.min$rvar
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
  
  df.ev <- data.frame(df.rv[, 1])
  names(df.ev) <- "date"
  
  y.names <- NA
  
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.sl <- paste("log.term.struct", var.tickers[j], sep = ".")
    name.matu <- paste("time.maturity", var.tickers[j], sep = ".")
    
    dummy.sl <- rep(0, n.df)
    sl <- Lag(df.sl[, name.sl] * 100, start.lag = n.ahead)
    dummy.sl[sl < 0] <- 1
    
    
    temp.df.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                         #dependent variable
                                   , Lag(0.5 * log(df.time.matu[, name.matu] / 86400) / 100, start.lag = 0)
                                   , sl
                                   , dummy.sl
                                   , dummy.sl * sl))
    
    
    names(temp.df.ev) <- c(paste0("rv.", var.tickers[j])
                           , paste0("dtm.", var.tickers[j])
                           , paste0("sl.", var.tickers[j])
                           , paste0("dum.sl.", var.tickers[j])
                           , paste0("dum.sl.interact.", var.tickers[j]))
    
    
    if(any(var.tickers[j] == c("C", "S", "W"))){
    
      temp.df.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                         #dependent variable
                                         , Lag(mat.calendar.dum[, 8], start.lag = 0)
                                         , Lag(0.5 * log(df.time.matu[, name.matu] / 86400) / 100, start.lag = 0)
                                         , sl
                                         , dummy.sl
                                         , dummy.sl * sl))
      
      
      names(temp.df.ev) <- c(paste0("rv.", var.tickers[j])
                                 , paste0(names(mat.calendar.dum)[8], ".", var.tickers[j])
                                 , paste0("dtm.", var.tickers[j])
                                 , paste0("sl.", var.tickers[j])
                                 , paste0("dum.sl.", var.tickers[j])
                                 , paste0("dum.sl.interact.", var.tickers[j]))
    }
    
    
      if(var.tickers[j] == "NG"){
      
      temp.df.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                         #dependent variable
                                     , Lag(mat.calendar.dum[, 2], start.lag = 0)
                                     , Lag(0.5 * log(df.time.matu[, name.matu] / 86400) / 100, start.lag = 0)
                                     , sl
                                     , dummy.sl
                                     , dummy.sl * sl))
      
      
      names(temp.df.ev) <- c(paste0("rv.", var.tickers[j])
                             , paste0(names(mat.calendar.dum)[2], ".", var.tickers[j])
                             , paste0("dtm.", var.tickers[j])
                             , paste0("sl.", var.tickers[j])
                             , paste0("dum.sl.", var.tickers[j])
                             , paste0("dum.sl.interact.", var.tickers[j]))
    }
    
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    
    temp.df.ev <- cleaner.df(temp.df.ev)
    df.ev <- cbind(df.ev, temp.df.ev)

    list.formula[[j]] <- formula(paste(paste(names(temp.df.ev)[1], paste0(names(temp.df.ev)[2:ncol(temp.df.ev)], collapse = " + "), sep = " ~ ")))     ##more tractable formula:
  
  }
  
  # print(list.formula)
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    df.ev <- df.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
  }

  if(out.sample.mode){
    df.ev.out <- df.ev[(training.sample.length + 1):n.df, ]
    df.ev <- df.ev[(1:training.sample.length), ]
  }
  
  #start estimation of the SUR
  mod.ev <- systemfit(list.formula, data = df.ev, method = "SUR")
  
  ##to return the model unselect below
  # return(mod.ev)
  
  t.extract <- extract(mod.ev, beside = TRUE, include.rsquared = FALSE)
  
  for(j in 1:9){

    t.extract[[j]]@coef.names <- c("Constant"
                                   , "$TM_{c,t} (10^{2})$"
                                   , "$SL_{c,t-1} (10^{-2})$"
                                   , "$B_{c,t-1}$"
                                   , "$SL_{c,t-1} * B_{c,t-1} (10^{-2})$")
    
    
    if(any(var.tickers[j] == c("C", "S", "W", "NG"))){
      
      t.extract[[j]]@coef.names <- c("Constant"
                                     , "$M_{c,t}$"
                                     , "$TM_{c,t} (10^{2})$"
                                     , "$SL_{c,t-1} (10^{-2})$"
                                     , "$B_{c,t-1}$"
                                     , "$SL_{c,t-1} * B_{c,t-1} (10^{-2})$")
      
    }
  }

  
  #to return the model summary, unselect below
  # print(summary(mod.ev))
  
  # print(texreg(t.extract
  #              , single.row = FALSE
  #          #   , custom.header = list("$RV_{c, t+1}$" = 1:9)
  #              , custom.model.names = var.tickers
  #              , stars = c(0.01, 0.05, 0.1)))
  
  # print(paste("ols r.squared", summary(mod.ev)$ols.r.squared))
  # print(paste("mcelroy r.squared", summary(mod.ev)$mcelroy.r.squared))
  # print(paste("log likelihood", logLik(mod.ev)))
  
  if(out.sample.mode){
    
    df.forecast <- predict(mod.ev, newdata = df.ev.out)
    df.real <- df.ev.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
    
  }
  
  if(!out.sample.mode){
    
    df.forecast <- predict(mod.ev)
    df.errors <- residuals(mod.ev)
    df.real <- df.forecast + df.errors
    
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}