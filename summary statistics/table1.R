#returns daily statistics for the various intraday data sampled at various frequencies
Table1 <- function(df = list.5.min$rvar
                   , mode = c("RV", "ARV")){
  
  library(stargazer)
  library(fracdiff)
  library(sandwich)
  library(lmtest)
  
  date <- df[, 1]
  df <- df[, -1]
  n <- dim(df)[1]
  
  #arithmetic realized volatility
  if(mode == "ARV"){
    
    df <- df^(1/2)
    df[df == 0] <- NA
  }
  
  #log realized volatility
  if(mode == "RV"){
    df <- 0.5 * log(df)
    
    #replace potential Inf values (not handled) by NAs
    df[abs(df) == Inf] <- NA
  }
 

  res <- data.frame(matrix(data = 0, nrow = 7, ncol = 9))
  
  my.names <- c("Corn (C)"
                , "Soybeans (S)"
                , "Wheat (W)"
                , "WTI crude oil (CL)"
                , "Heating oil (HO)"
                , "Natural Gas (NG)"
                , "Gold (GC)"
                , "Copper (HG)"
                , "Silver (SI)")
  
  colnames(res) <- my.names
  
  for(j in 1:9){
    
    t.df <- df[, j]
    res[, j] <- InLineStats1(t.df)
  }
  
  stargazer(res, summary = FALSE, digits = 2, digits.extra = 2)
}

InLineStats1 <- function(vec){
  
  library(fBasics)
  #get length
  n <- length(vec)
  
  
  my.mean <- mean(vec, na.rm = T) * 100
  my.sd <- sd(vec, na.rm = T) * 100
  
  my.skew <- skewness(vec, na.rm = T)
  my.kurto <- kurtosis(vec, na.rm = T)

  #########
  #add Ljung Box portmantau test
  #using the same lag as Andersen et al. (2003), i.e., 20
  #########
  my.box.ljung <- as.numeric(Box.test(vec, lag = 20, type = "Ljung-Box")$statistic)
  
  #########
  #add ARFIMA determination of parameter d for fractional integration
  #using the same bandwith parameter as Andersen et al. (2003), i.e. ^(4/5)
  my.d <- fdGPH(na.omit(vec), bandw.exp = 4/5)$d
  #########

  ### auto correlation function
  # dev.new()
  # acf(rv, lag.max = 400, main = ticker)
  ###
  
  ### kernel density plot and normal density
  # dev.new()
  ### normalize density
  my.norm <- (na.omit(vec) - mean(vec, na.rm = T)) / sd(vec, na.rm = T)
  
  my.jb <- as.numeric(jarqueberaTest(my.norm)@test$statistic)
  
  # print(jarqueberaTest(my.norm))
  
  ### plot normal density
  # plot(seq(-10, 10, by = 0.001), dnorm(seq(-10, 10, by = 0.001)), t = 'l', lty = "dashed", ylim = c(0, max(c(density(rv.norm)$y, dnorm(seq(-10, 10, by = 0.001))))), main = ticker)
  
  ### add kernel density of rv
  # lines(density(rv.norm, kernel = "gaussian"), main = ticker, col = 'red')
  # lines()
  # lines(dnorm(0, 1))
  
  # print(coeftest(mod, vcov = NeweyWest(mod, verbose = TRUE)))
  # rv.t <- coeftest(mod, vcov = NeweyWest(mod, verbose = TRUE))[2, 3]    #tStat with Newey-West 1994, automatic lag selection
  # rv.rsq <- summary(mod)$r.squared * 100
  
  # contango <- (sum(df[, term.col] > 0, na.rm = T) / length(na.omit(df[, term.col]))) * 100
  # print("contango: ")
  # print(contango)
  
  return(c(my.mean
           , my.sd
           , my.skew
           , my.kurto
           , my.jb
           , my.box.ljung
           , my.d))
}


LjungBox <- function(s, win){
  
  q.vec <- rep(NA, win)
  
  for(i in 1:win){
    q.vec[i] <- as.numeric(Box.test(s, lag = i, type = "Ljung-Box")$statistic)
  }
  
  dev.new()
  plot(q.vec, t = 'l')
}

DensityPlots <- function(s){
}

##report statistics of mean difference of log realized volatility, conditional on the term structure slope for individual commodities and for the whole sample
ConditionalVol <- function(df.rv, df.sl){
  
  library(stargazer)
  
  #to format the t-stat output: trace(stargazer:::.stargazer.wrap, edit = T) go to ligne 7105 and 7106. replace by parenthesis
  n.df <- dim(df.rv)[1]
  p.df <- dim(df.rv)[2]
  
  var.tickers <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI")
  
  contango.se <- rep(NA, 9)
  contango.mean <- rep(NA, 9)
  
  backwardation.se <- rep(NA, 9)
  backwardation.mean <- rep(NA, 9)
  
  # contract.t <- list()
  contract.t <- rep(NA, 9)
  
  all.backwardation <- NA
  all.contango <- NA
  
  for(j in 1:9){
    
    name.y <- paste("rvar.288", var.tickers[j], sep = ".")
    name.sl <- paste("log.term.struct", var.tickers[j], sep = ".")
    
    vec.rv.contango <- df.rv[, name.y][df.sl[, name.sl] > 0]
    vec.rv.backwardation <- df.rv[, name.y][df.sl[, name.sl] < 0]
    
    vec.rv.backwardation[abs(vec.rv.backwardation) == Inf] <- NA
    vec.rv.contango[abs(vec.rv.contango) == Inf] <- NA
  
    all.backwardation <- c(all.backwardation, vec.rv.backwardation)
    all.contango <- c(all.contango, vec.rv.contango)
    
    contract.t[j] <- t.test(vec.rv.contango, vec.rv.backwardation)$statistic
  }
  
  print(t.test(all.contango, all.backwardation))
  return(rbind(var.tickers, contract.t))
}

