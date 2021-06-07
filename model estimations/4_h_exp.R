## Methodology of Bollerslev, Hood, Huss and Pedersen (2018) with exponential smoothing of log RV
HarExp <- function(df.rv = list.5.min$rvar
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
  
  matrix.residuals <- matrix(data = NA, nrow = n.df, ncol = 9)
  
  df.hexp <- data.frame(df.rv[, 1])
  names(df.hexp) <- "date"
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    
    temp.df.hexp <- data.frame(cbind(0.5 * log(df.rv[, name.y])                       #dependent variable
                                    , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 1, win = 500)), start.lag = n.ahead - 1)
                                    , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 5, win = 500)), start.lag = n.ahead - 1)
                                    , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 25, win = 500)), start.lag = n.ahead - 1)
                                    , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 125, win = 500)), start.lag = n.ahead - 1))) #hexp
    
    names(temp.df.hexp) <- c(paste0("rv.", var.tickers[j])
                            , paste0("hexp1.", var.tickers[j])
                            , paste0("hexp5.", var.tickers[j])
                            , paste0("hexp25.", var.tickers[j])
                            , paste0("hexp125.", var.tickers[j]))
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    temp.df.hexp <- cleaner.df(temp.df.hexp)
    df.hexp <- cbind(df.hexp, temp.df.hexp)
    list.formula[[j]] <- formula(paste(names(temp.df.hexp)[1], paste(names(temp.df.hexp)[-1], collapse = " + "), sep = " ~ "))
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    df.hexp <- df.hexp[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
  }
  
  if(out.sample.mode){
    
    df.hexp.out <- df.hexp[(training.sample.length + 1):n.df, ]
    df.hexp <- df.hexp[(1:training.sample.length), ]
  }
  
  #start estimation of the SUR
  mod.hexp <- systemfit(list.formula, data = df.hexp, method = "SUR")
  
  ##To return the model, uncomment below
  # return(mod.hexp)
  
  t.extract <- extract(mod.hexp, beside = TRUE, include.rsquared = FALSE)
  
  for(j in 1:9){
    
    t.extract[[j]]@coef.names <- c("Constant"
                                   , "$RV^{CoM1}_{c,t-1}$"
                                   , "$RV^{CoM5}_{c,t-1}$"
                                   , "$RV^{CoM25}_{c,t-1}$"
                                   , "$RV^{CoM125}_{c,t-1}$")
  }
  
  ##print the model summary
  print(texreg(t.extract
               , single.row = FALSE
               #, custom.header = list("$RV_{c, t+1}$" = 1:9)
               , custom.model.names = var.tickers
               , stars = c(0.01, 0.05, 0.1)))
  
  
  print(paste("ols r.squared", summary(mod.hexp)$ols.r.squared))
  print(paste("mcelroy r.squared", summary(mod.hexp)$mcelroy.r.squared))
  print(paste("log likelihood", logLik(mod.hexp)))
  
  
  if(out.sample.mode){
    
    df.forecast <- predict(mod.hexp, newdata = df.hexp.out)
    df.real <- df.hexp.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
  
  if(!out.sample.mode){
    
    df.forecast <- predict(mod.hexp)
    df.errors <- residuals(mod.hexp)
    df.real <- df.forecast + df.errors
    
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}


HExp <- function(rv, center.mass = c(1, 5, 25, 125), win){
  
  n <- length(rv)
  ## center of mass gives a lambda of
  lambda <- log(1 + 1 / center.mass)
  
  exp.rv <- rep(NA, n)
  
  for(i in (win + 2):(n - 1)){
    exp.rv[i] <- StaticHexp(rv[(i - win - 1) :(i - 1)], lambda)
  }
  
  return(exp.rv)
}

StaticHexp <- function(s, lambda){
  
  n <- length(s)
  hexp.s <- 0
  
  ##first compute the denominator
  denom <- 0
  for(i in 1:n){
    denom <- denom + exp(- (i * lambda))
  }
  
  for(i in 1:n){
    hexp.s <- hexp.s + (exp(-i * lambda) / denom) * s[(n - i + 1)]
  }
  
  return(hexp.s)
}