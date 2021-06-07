#in sample estimation
HarExpEv <- function(df.rv = list.5.min$rvar
                     , df.sl = list.5.min$slope
                     , df.time.matu = list.5.min$time.matu
                     , mat.calendar.dum = list.5.min$month.dummies
                     , center.mass = c(1, 5, 25, 125)
                     , win = 500
                     , out.sample.mode = FALSE
                     , training.sample.length = 1200
                     , n.ahead = c(1, 5, 22)){
  
  library(stargazer)
  #to format the t-stat output: trace(stargazer:::.stargazer.wrap, edit = T) go to ligne 7105 and 7106. replace by parenthesis
  library(texreg)
  
  n.df <- dim(df.rv)[1]
  p.df <- dim(df.rv)[2]
  
  var.tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  list.models <- list()
  list.formula <- list()
  names.y <- names(df.rv)
  
  df.hexp.ev <- data.frame(df.rv[, 1])
  names(df.hexp.ev) <- "date"
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.sl <- paste("log.term.struct", var.tickers[j], sep = ".")
    name.matu <- paste("time.maturity", var.tickers[j], sep = ".")
    
    dummy.sl <- rep(0, n.df)
    sl <- Lag(df.sl[, name.sl] * 100, start.lag = n.ahead)
    dummy.sl[sl < 0] <- 1
    
    temp.df.hexp.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                       , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 1, win = win)), start.lag = n.ahead - 1)          #day lag
                                       , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 5, win = win)), start.lag = n.ahead - 1)
                                       , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 25, win = win)), start.lag = n.ahead - 1)
                                       , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 125, win = win)), start.lag = n.ahead - 1)
                                       , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, start.lag = 0)
                                       , sl
                                       , dummy.sl
                                       , dummy.sl * sl))
  
    
    names(temp.df.hexp.ev) <- c(paste0("rv.", var.tickers[j])
                               , paste0("hexp1.", var.tickers[j])
                               , paste0("hexp5.", var.tickers[j])
                               , paste0("hexp25.", var.tickers[j])
                               , paste0("hexp125.", var.tickers[j])
                               , paste0("dtm.", var.tickers[j])
                               , paste0("sl.", var.tickers[j])
                               , paste0("dum.sl.", var.tickers[j])
                               , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
    
    if(any(var.tickers[j] == c("C", "S", "W"))){
      
      temp.df.hexp.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 1, win = win)), start.lag = n.ahead - 1)          #day lag
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 5, win = win)), start.lag = n.ahead - 1)
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 25, win = win)), start.lag = n.ahead - 1)
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 125, win = win)), start.lag = n.ahead - 1)
                                          , Lag(mat.calendar.dum[, 8], start.lag = 0)
                                          , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, start.lag = 0)
                                          , sl
                                          , dummy.sl
                                          , dummy.sl * sl))
      
      
      names(temp.df.hexp.ev) <- c(paste0("rv.", var.tickers[j])
                                  , paste0("hexp1.", var.tickers[j])
                                  , paste0("hexp5.", var.tickers[j])
                                  , paste0("hexp25.", var.tickers[j])
                                  , paste0("hexp125.", var.tickers[j])
                                  , paste0(names(mat.calendar.dum)[8], ".", var.tickers[j])
                                  , paste0("dtm.", var.tickers[j])
                                  , paste0("sl.", var.tickers[j])
                                  , paste0("dum.sl.", var.tickers[j])
                                  , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
      
      
    }
    
    if(var.tickers[j] == "NG"){
     
      temp.df.hexp.ev <- data.frame(cbind(0.5 * log(df.rv[, name.y])
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 1, win = win)), start.lag = n.ahead - 1)          #day lag
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 5, win = win)), start.lag = n.ahead - 1)
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 25, win = win)), start.lag = n.ahead - 1)
                                          , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 125, win = win)), start.lag = n.ahead - 1)
                                          , Lag(mat.calendar.dum[, 2], start.lag = 0)
                                          , Lag(0.5 * log(LagMean(df.time.matu[, name.matu], start.lag = 0, end.lag = 0) / 86400 + 1) / 100, start.lag = 0)
                                          , sl
                                          , dummy.sl
                                          , dummy.sl * sl))
      
      
      names(temp.df.hexp.ev) <- c(paste0("rv.", var.tickers[j])
                                  , paste0("hexp1.", var.tickers[j])
                                  , paste0("hexp5.", var.tickers[j])
                                  , paste0("hexp25.", var.tickers[j])
                                  , paste0("hexp125.", var.tickers[j])
                                  , paste0(names(mat.calendar.dum)[2], ".", var.tickers[j])
                                  , paste0("dtm.", var.tickers[j])
                                  , paste0("sl.", var.tickers[j])
                                  , paste0("dum.sl.", var.tickers[j])
                                  , paste0("dum.sl.interact.", var.tickers[j]))  ##more tractable data frame column names)
      
    }
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    temp.df.hexp.ev <- cleaner.df(temp.df.hexp.ev)
    df.hexp.ev <- cbind(df.hexp.ev, temp.df.hexp.ev)
    list.formula[[j]] <- formula(paste(paste(names(temp.df.hexp.ev)[1], paste0(names(temp.df.hexp.ev)[2:ncol(temp.df.hexp.ev)], collapse = " + "), sep = " ~ ")))     ##more tractable formula:
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    df.hexp.ev <- df.hexp.ev[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
  }

  
  if(out.sample.mode){
    df.hexp.ev.out <- df.hexp.ev[(training.sample.length + 1):n.df, ]
    df.hexp.ev <- df.hexp.ev[(1:training.sample.length), ]
  }
  
  #start estimation of the SUR
  mod.hexp.ev <- systemfit(list.formula, data = df.hexp.ev, method = "SUR")
  
  #To return the model, uncomment below
  # return(mod.hexp.ev)
  
  
  t.extract <- extract(mod.hexp.ev, beside = TRUE, include.rsquared = FALSE)
  
  for(j in 1:9){
    
    t.extract[[j]]@coef.names <- c("Constant"
                                   , "$RV^{CoM1}_{c,t-1}$"
                                   , "$RV^{CoM5}_{c,t-1}$"
                                   , "$RV^{CoM25}_{c,t-1}$"
                                   , "$RV^{CoM125}_{c,t-1}$"
                                   , "$TM_{c,t} (10^{2})$"
                                   , "$SL_{c,t-1} (10^{-2})$"
                                   , "$B_{c,t-1}$"
                                   , "$SL_{c,t-1} \\times B_{c,t-1} (10^{-2})$")
    
    if(any(var.tickers[j] == c("C", "S", "W", "NG"))){
      
      t.extract[[j]]@coef.names <- c("Constant"
                                     , "$RV^{CoM1}_{c,t-1}$"
                                     , "$RV^{CoM5}_{c,t-1}$"
                                     , "$RV^{CoM25}_{c,t-1}$"
                                     , "$RV^{CoM125}_{c,t-1}$"
                                     , "$M_{c,t}$"
                                     , "$TM_{c,t} (10^{2})$"
                                     , "$SL_{c,t-1} (10^{-2})$"
                                     , "$B_{c,t-1}$"
                                     , "$SL_{c,t-1} \\times B_{c,t-1} (10^{-2})$")
    }
  }
  
  ##To print model summary, uncomment below
  # print(texreg(t.extract
  #              , single.row = FALSE
  #              #, custom.header = list("$RV_{c, t+1}$" = 1:9)
  #              , custom.model.names = var.tickers
  #              , stars = c(0.01, 0.05, 0.1)))
  # 
  # 
  # print(paste("ols r.squared", summary(mod.hexp.ev)$ols.r.squared))
  # print(paste("mcelroy r.squared", summary(mod.hexp.ev)$mcelroy.r.squared))
  # print(paste("log likelihood", logLik(mod.hexp.ev)))
  
  
  
  if(out.sample.mode){
    
    df.forecast <- predict(mod.hexp.ev, newdata = df.hexp.ev.out)
    df.real <- df.hexp.ev.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
    
  }
  
  if(!out.sample.mode){
    
    df.forecast <- predict(mod.hexp.ev)
    df.errors <- residuals(mod.hexp.ev)
    df.real <- df.forecast + df.errors
    
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}