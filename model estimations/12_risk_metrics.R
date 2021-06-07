##estimate the RiskMetrics model
RiskMetrics <- function(df.rv = list.5.min$rvar
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
    
    df.rm <- data.frame(df.rv[, 1])
    names(df.rm) <- "date"
    
    y.names <- NA
    
    for(j in 1:9){
      
      name.y <- paste("rvar.288", var.tickers[j], sep = ".")
      temp.df.rm <- data.frame(cbind(0.5 * log(df.rv[, name.y])                       #dependent variable
                                       , Lag(0.5 * log(HExp(df.rv[, name.y], center.mass = 16, win = 500)), start.lag = n.ahead - 1)))
      
      names(temp.df.rm) <- c(paste0("rv.", var.tickers[j])
                               , paste0("risk.metrics.", var.tickers[j]))
      
      
      y.names <- c(y.names, paste0("rv.", var.tickers[j]))
      
      temp.df.rm <- cleaner.df(temp.df.rm)
      df.rm <- cbind(df.rm, temp.df.rm)
      
      list.formula[[j]] <- formula(paste(names(temp.df.rm)[1], paste(names(temp.df.rm)[-1], collapse = " + "), sep = " ~ "))
    }
    
    
    y.names <- y.names[-1]
    
    if(!out.sample.mode){
      df.rm <- df.rm[523:n.df, ] ## use 500 to keep number of observations comparable to the rm
    }
    
    if(out.sample.mode){
      
      df.rm.out <- df.rm[(training.sample.length + 1):n.df, ]
      df.rm <- df.rm[(1:training.sample.length), ]
    }
    
    mod.rm <- systemfit(list.formula, data = df.rm, method = "SUR")

    t.extract <- extract(mod.rm, beside = TRUE, include.rsquared = FALSE)
    
    for(j in 1:9){
      
      t.extract[[j]]@coef.names <- c("Constant", "$risk.metrics$")
    }
    
    
    ##print model summary
    print(texreg(t.extract
                 , single.row = FALSE
                 , custom.header = list("$RV_{c, t+1}$" = 1:9)
                 , custom.model.names = var.tickers
                 , stars = c(0.01, 0.05, 0.1)))


    print(paste("ols r.squared", summary(mod.rm)$ols.r.squared))
    print(paste("mcelroy r.squared", summary(mod.rm)$mcelroy.r.squared))
    print(paste("log likelihood", logLik(mod.rm)))
    

    if(out.sample.mode){
      
      df.forecast <- predict(mod.rm, newdata = df.rm.out)
      df.real <- df.rm.out[, y.names]
      df.errors <- df.real - df.forecast
      return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
      
    }
    
    if(!out.sample.mode){
      
      df.forecast <- predict(mod.rm)
      df.errors <- residuals(mod.rm)
      df.real <- df.forecast + df.errors
      return(list(df.real = df.real, df.forecast = df.forecast, df.errors = df.errors))
    }
  }