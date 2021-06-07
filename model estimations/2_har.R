#in sample estimation
Har <- function(df.rv = list.5.min$rvar
                , out.sample.mode = FALSE
                , training.sample.length = 1200
                , n.ahead = c(1, 5, 22)){
  
  library(systemfit)
  library(texreg)
  
  n.df <- dim(df.rv)[1]
  p.df <- dim(df.rv)[2]
  
  var.tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  
  list.models <- list()
  list.formula <- list()
  
  df.har <- data.frame(df.rv[, 1])
  df.rv[df.rv == 0] <- NA
  
  names(df.har) <- "date"
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")

    temp.df.har <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                           #dependent variable
                               , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)           #day lag
                               , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)           #week lag
                               , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)))         #month lag
    
    
    names(temp.df.har) <- c(paste0("rv.", var.tickers[j])
                               , paste0("har1.", var.tickers[j])
                               , paste0("har5.", var.tickers[j])
                               , paste0("har22.", var.tickers[j]))
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    
    temp.df.har <- cleaner.df(temp.df.har)
    df.har <- cbind(df.har, temp.df.har)
    
    list.formula[[j]] <- formula(paste(names(temp.df.har)[1], paste(names(temp.df.har)[-1], collapse = " + "), sep = " ~ "))
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    df.har <- df.har[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
  }

  if(out.sample.mode){
    
    df.har.out <- df.har[(training.sample.length + 1):n.df, ]
    df.har <- df.har[(1:training.sample.length), ]
  }
  
  #start estimation of the SUR
  mod.har <- systemfit(list.formula, data = df.har, method = "SUR")
  
  ##to return the model, unselect below
  # return(mod.har)
  
  t.extract <- extract(mod.har, beside = TRUE, include.rsquared = FALSE)

  for(j in 1:9){

    t.extract[[j]]@coef.names <- c("Constant"
                                   , "$RV_{c,t-1}$"
                                   , "$RV_{c,t-2|t-5}$"
                                   , "$RV_{c,t-6|t-22}$")
  }

  ##print the model summary
    
  print(texreg(t.extract
                , single.row = FALSE
                #, custom.header = list("$RV_{c,t}$" = 1:9)
                , custom.model.names = var.tickers
                , stars = c(0.01, 0.05, 0.1)))


  print(paste("ols r.squared", summary(mod.har)$ols.r.squared))
  print(paste("mcelroy r.squared", summary(mod.har)$mcelroy.r.squared))
  print(paste("log likelihood", logLik(mod.har)))
  
  
  if(out.sample.mode){
    
    df.forecast <- predict(mod.har, newdata = df.har.out)
    df.real <- df.har.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
  
  if(!out.sample.mode){
    
    df.forecast <- predict(mod.har)
    df.errors <- residuals(mod.har)
    df.real <- df.forecast + df.errors
    
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}