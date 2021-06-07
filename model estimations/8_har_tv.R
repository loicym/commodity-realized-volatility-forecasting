#for each specifications, same output as HAR models, with list of predictions at 1, 5, and 22 days + forecast errors
HarTv <- function(df.rv = list.5.min$rvar
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
    
    df.har.tv <- data.frame(df.rv[, 1])
    names(df.har.tv) <- "date"
    
    y.names <- NA
    
    for(j in 1:9){
      
      name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    
      temp.df.har.tv <- data.frame(cbind(0.5 * log(df.rv[, name.y])                                                           #dependent variable
                                      , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 1, end.lag = 1)), start.lag = n.ahead - 1)           #day lag
                                      , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 5, end.lag = 2)), start.lag = n.ahead - 1)           #week lag
                                      , Lag(0.5 * log(LagMean(df.rv[, name.y], start.lag = 22, end.lag = 6)), start.lag = n.ahead - 1)))         #month lag
      
      
      names(temp.df.har.tv) <- c(paste0("rv.", var.tickers[j])
                              , paste0("har1.", var.tickers[j])
                              , paste0("har5.", var.tickers[j])
                              , paste0("har22.", var.tickers[j]))
      
      y.names <- c(y.names, paste0("rv.", var.tickers[j]))
      
      temp.df.har.tv <- cleaner.df(temp.df.har.tv)
      df.har.tv <- cbind(df.har.tv, temp.df.har.tv)
      
      list.formula[[j]] <- formula(paste(names(temp.df.har.tv)[1], paste(names(temp.df.har.tv)[-1], collapse = " + "), sep = " ~ "))
    }
    
    y.names <- y.names[-1]
    
    if(!out.sample.mode){
      
      # df.har.tv <- df.har.tv[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
      ###BEWARE!! save to global environment with <<-
      df.har.tv <- df.har.tv[523:n.df, ] ## use 500 to keep number of observations comparable to the hexp
      df.har.tv <<- df.har.tv
      
    }

    if(out.sample.mode){
      
      df.har.tv.out <- df.har.tv[(training.sample.length + 1):n.df, ]
      df.har.tv <- df.har.tv[(523:training.sample.length), ]
      
      df.har.tv.out <<- df.har.tv.out
      df.har.tv <<- df.har.tv
      
    }
    
    #start estimation of the TV-SUR
    mod.har.tv <- tvSURE(formula = list.formula, data = df.har.tv, method = "tvFGLS", control = list(tol = 0.1, maxiter = 10))
    
    ##to return the model, uncomment below
    # return(mod.har.tv)
    
    #create a data frame to fill with extracted results:
    res.df.tex <- data.frame(matrix(data = NA, nrow = 10, ncol = 9))
    
    rownames(res.df.tex) <- c("Constant", "1"
                              , "$RV_{c,t-1}$", "2"
                              , "$RV_{c,t-2|t-5}$", "3"
                              , "$RV_{c,t-6|t-22}$", "4"
                              , "Bandwidth"
                              , "Pseudo R^{2}")
    
    k <- 1
    for(j in 1:9){
      i <- 1
      #get matrix of time varying coeffs
      while(i < 8){
        
        #report coefficients
        # print(mean(mod.har.tv$coefficients[, k]))
        
        res.df.tex[i, j] <- mean(mod.har.tv$coefficients[, k])
        # print(typeof(mod.har.tv$coefficients[, k]))
        
        #report standard error of coefficients
        res.df.tex[(i + 1), j] <- sd(mod.har.tv$coefficients[, k], na.rm = T) / length(mod.har.tv$coefficients[, k])^0.5
        #format coefficients 
        res.df.tex[i, j] <- ApplyStars(my.coeff = res.df.tex[i, j], my.se = res.df.tex[(i + 1), j])
        #format standard errors
        res.df.tex[(i + 1), j] <- ApplyBrackets(my.se = res.df.tex[(i + 1), j])
        #switch to new time varying coefficient
        k <- k + 1
        #switch to next two rows in the reporting table
        i <- i + 2
      }
      
      res.df.tex[9, j] <- ApplyDollars(my.val = mod.har.tv$bw[j])
      res.df.tex[10, j] <- ApplyDollars(my.val = 1 - (var(mod.har.tv$residuals[, j]) / var(mod.har.tv$y[, j])))
    }
    
    ##print (raw) summary
    print(res.df.tex)
    
    # return(stargazer(res.df.tex, summary = FALSE, digits = 2, digits.extra = 2))
    
    if(out.sample.mode){
      
      df.forecast <- forecast(mod.har.tv, newdata = df.har.tv.out, n.ahead = dim(df.har.tv.out)[1])
      df.real <- df.har.tv.out[, y.names]
      df.errors <- df.real - df.forecast
      return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
    }
    
    if(!out.sample.mode){
      
      df.forecast <- mod.har.tv$fitted
      df.errors <- mod.har.tv$residuals
      df.real <- mod.har.tv$y
      return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
    }
}



##Set of functions to present results and table from output of TV-SUR environment
ApplyStars <- function(my.coeff, my.se){
  
  my.coeff <- as.double(my.coeff)
  my.se <- as.double(my.se)
  
  if((abs(my.coeff) / my.se) < 1.64){
    return(paste0("&", "$", format(round(my.coeff, 2), nsmall = 2), "$"))
  }
  
  if((abs(my.coeff) / my.se) < 1.96){
    return(paste0("&", "$", format(round(my.coeff, 2), nsmall = 2), "^{*}", "$"))
  }
  
  if((abs(my.coeff) / my.se) < 2.56){
    return(paste0("&", "$", format(round(my.coeff, 2), nsmall = 2), "^{**}", "$"))
  }
  
  return(paste0("&", "$", format(round(my.coeff, 2), nsmall = 2), "^{***}", "$"))
}

ApplyBrackets <- function(my.sd){
  
  my.sd <- format(round(as.double(my.sd), 2), nsmall = 2)
  return(paste0("&", "$", "[", my.sd, "]", "$"))
}

ApplyMinMax <- function(my.min, my.max){
  
  my.min <- format(round(as.double(my.min), 2), nsmall = 2)
  my.max <- format(round(as.double(my.max), 2), nsmall = 2)
  
  return(paste0("&", "$", "[", my.min, " / ", my.max, "]", "$"))
}

ApplyParenthesis <- function(my.se){
  
  my.se <- format(round(as.double(my.se), 2), nsmall = 2)
  return(paste0("&", "$", "(", my.se, ")", "$"))
  
}

ApplyDollars <- function(my.val){
  
  my.val <- format(round(as.double(my.val), 2), nsmall = 2)
  return(paste0("&", "$", my.val, "$"))
}
