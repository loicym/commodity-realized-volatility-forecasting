#for each specifications, same output as HAR models, with list of predictions at 1, 5, and 22 days + forecast errors
HarTvQ <- function(df.rv = list.5.min$rvar
                  , out.sample.mode = FALSE
                  , training.sample.length = 1200
                  , n.ahead = c(1, 5, 22)){
  
  library(systemfit)
  library(texreg)
  library(tvReg)
  library(stargazer)
  
  n.df <- dim(df.rv)[1]
  p.df <- dim(df.rv)[2]
  
  var.tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  list.models <- list()
  list.formula <- list()
  
  df.har.tv.q <- data.frame(df.rv[, 1])
  names(df.har.tv.q) <- "date"
  
  y.names <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.rq <- paste("realized.log.quarticity.288", var.tickers[j], sep = ".")
    
    print(name.y)
    temp.df.har.tv.q <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                           #dependent variable
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)           #day lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1) * Lag(0.5 * LagMean(df.rv[, name.rq], start.lag = 1, end.lag = 1), start.lag = n.ahead - 1)#day lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)           #week lag
                                       , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)))         #month lag
    
    names(temp.df.har.tv.q) <- c(paste0("rv.", var.tickers[j])
                               , paste0("har1.", var.tickers[j])
                               , paste0("har.rq.1", var.tickers[j])
                               , paste0("har5.", var.tickers[j])
                               , paste0("har22.", var.tickers[j]))
    
    
    y.names <- c(y.names, paste0("rv.", var.tickers[j]))
    temp.df.har.tv.q <- cleaner.df(temp.df.har.tv.q)
    df.har.tv.q <- cbind(df.har.tv.q, temp.df.har.tv.q)
    list.formula[[j]] <- formula(paste(names(temp.df.har.tv.q)[1], paste(names(temp.df.har.tv.q)[-1], collapse = " + "), sep = " ~ "))
  }
  
  y.names <- y.names[-1]
  
  if(!out.sample.mode){
    # df.har.tv.q <- df.har.tv.q[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
    ###BEWARE!! save to global environment with <<-
    df.har.tv.q <- df.har.tv.q[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
    df.har.tv.q <<- df.har.tv.q
  }

  if(out.sample.mode){
    
    df.har.tv.q.out <- df.har.tv.q[(training.sample.length + 1):n.df, ]
    df.har.tv.q <- df.har.tv.q[(523:training.sample.length), ]
    
    df.har.tv.q.out <<- df.har.tv.q.out
    df.har.tv.q <<- df.har.tv.q
  }
  
  #start estimation of the TV-SUR
  mod.har.tv.q <- tvSURE(formula = list.formula, data = df.har.tv.q, method = "tvFGLS", control = list(tol = 0.001, maxiter = 10))

  ##to return the model, uncomment below
  # return(mod.har.tv.q)
  
  #create a data frame to fill with extracted results:
  res.df.tex <- data.frame(matrix(data = NA, nrow = 12, ncol = 9))
  
  
  rownames(res.df.tex) <- c("Constant", "1"
                            , "$RV_{c,t-1}$", "2"
                            , "$RV_{c,t-1} \times RQ_{c,t-1}$", "3"
                            , "$RV_{c,t-2|t-5}$", "4"
                            , "$RV_{c,t-6|t-22}$", "5"
                            , "Bandwidth"
                            , "Pseudo R^{2}")
  

  k <- 1
  
  for(j in 1:9){
    
    i <- 1
    #get matrix of time varying coeffs
    while(i < 10){
      
      #report coefficients
      print(mean(mod.har.tv.q$coefficients[, k]))
      res.df.tex[i, j] <- mean(mod.har.tv.q$coefficients[, k])
      print(typeof(mod.har.tv.q$coefficients[, k]))
      #report standard error of coefficients
      res.df.tex[(i + 1), j] <- sd(mod.har.tv.q$coefficients[, k], na.rm = T) / length(mod.har.tv.q$coefficients[, k])^0.5
      #format coefficients 
      res.df.tex[i, j] <- ApplyStars(my.coeff = res.df.tex[i, j], my.se = res.df.tex[(i + 1), j])
      #format standard errors
      res.df.tex[(i + 1), j] <- ApplyBrackets(my.se = res.df.tex[(i + 1), j])
      #switch to new time varying coefficient
      k <- k + 1
      #switch to next two rows in the reporting table
      i <- i + 2
    }
    
    res.df.tex[11, j] <- ApplyDollars(my.val = mod.har.tv.q$bw[j])
    res.df.tex[12, j] <- ApplyDollars(my.val = 1 - (var(mod.har.tv.q$residuals[, j]) / var(mod.har.tv.q$y[, j])))
  }

  ##print summary of the TV model  
  print(res.df.tex)

  ##stargazer test (not run)
  # stargazer(res.df.tex, summary = FALSE, digits = 2, digits.extra = 2)
  
  if(out.sample.mode){
    
    df.forecast <- forecast(mod.har.tv.q, newdata = df.har.tv.q.out, n.ahead = dim(df.har.tv.q.out)[1])
    df.real <- df.har.tv.q.out[, y.names]
    df.errors <- df.real - df.forecast
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
  
  if(!out.sample.mode){
    
    df.forecast <- mod.har.tv.q$fitted
    df.errors <- mod.har.tv.q$residuals
    df.real <- mod.har.tv.q$y
    return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
  }
}