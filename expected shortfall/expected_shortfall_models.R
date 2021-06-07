#script for expected shortfall:
#need log returns at daily frequency matching with:
#variance forecasts from log(RV) forecasts from all models
#big.b is number of permutations of bootstrap
#nb.risk.levels is the number of risk levels considered between tau = 0.975 and 1
#dof is the degree of freedom

ExpectedShortfall <- function(big.b = big.b
                              , vec.forecasts = vec.forecasts
                              , vec.ret = vec.ret
                              , dof = dof
                              , p = p){       # Number of risk levels (to be set by the user)
  


  
  s.t = exp(2 * vec.forecasts)                               # convert forecasts in conditional variance
  vec.ret = -vec.ret                                         # convert returns in losses (same as Leymarie 2019)
  n <- length(vec.ret)
  tau <- 0.975;                                              # Coverage level of ES                                       
  
  u <- matrix(data = NA, nrow = p, ncol = 1)
  
  for (i in 1:p){
    u[i, 1] <- tau + (i - 1) / p * (1 - tau)                # Risk levels u_j, j = 1,2, ..., p
  }
  
  es.t <- matrix(data = NA, nrow = n, ncol = 1)                             # Initialization of Expected Shortfall (exact calculation method)

  nb.std.innov <- 100000;                                                   # Number of pseudo standardized innovations

  
  eta <- rt(n = nb.std.innov, df = dof) / sqrt(dof / (dof - 2))             # Pseudo standardized innovations
  f.v.eta <- quantile(eta, tau)                                             # Empirical tau-quantile of the standardized innovations
  m.eta <- sum(eta[eta > f.v.eta]) / sum(eta > f.v.eta)                     # ES of the standardized innovations
  es.t[, 1] <- (s.t^0.5) * m.eta                                            # ES estimates at coverage level tau (with exact calculation method)
  
  # for (i in 1:n){                                                         # Loop of time t
  # }# End of loop i
  
  # dev.new()
  plot(vec.ret, t = 'l')
  lines(es.t, col = 'red')

  var.t.temp <- matrix(data = sqrt(s.t), nrow = n, ncol = p)
  stat.temp <- matrix(qt(p = t(u), df = dof) / sqrt(dof / (dof - 2)), nrow = n, ncol = p, byrow = TRUE)
 
  var.t <- var.t.temp * stat.temp
  
  # print(head(var.t))
  # print(tail(var.t))
  
  #=====================================#
  #== Multi-Quantile Regression (MQR) ==#
  #=====================================#
  b.hat <- matrix(data = NA, nrow = 2, ncol = p)                                      # Initialization of the MQR parameters        
  quantile.reg.stats <- matrix(data = NA, nrow = 6 * p, ncol = 1)
  k <- 1
  
  # dev.new()
  plot(vec.ret, t = 'l')
  
  es.violations <- matrix(data = NA, nrow = length(vec.ret), ncol = p)
  
  
  for(j in 1:p){
    
    temp.rq.reg <- rq(vec.ret~var.t[, j], tau = u[j, 1])
    temp.lm.reg <- lm(vec.ret~var.t[, j])
    temp.vcov <- vcov(temp.lm.reg)
    
    es.violations[, j] <- predict(temp.rq.reg)
    
    lines(rowMeans(es.violations, na.rm = T), col = 'red')
    print(paste0("tau level is p = ", j))
    
    b.hat[, j] <- as.numeric(coefficients(temp.rq.reg))   # MQR parameter estimates
    
    
    lin.hyp <- linearHypothesis(temp.rq.reg, c("(Intercept) = 0", "var.t[, j] = 1"), vcov. = temp.vcov, test = "Chisq")
  
    quantile.reg.stats[k, 1] <- summary(temp.rq.reg)[[3]][1, 1] * 100 #report Beta_0
    quantile.reg.stats[(k + 1), 1] <- summary(temp.rq.reg)[[3]][1, 2] * 100 #report standard error of Beta_0
    quantile.reg.stats[(k + 2), 1] <- summary(temp.rq.reg)[[3]][2, 1] * 100#report Beta_1
    quantile.reg.stats[(k + 3), 1] <- summary(temp.rq.reg)[[3]][2, 2] * 100 #report standard error of Beta_1
    quantile.reg.stats[(k + 4), 1] <- as.numeric(lin.hyp$Chisq[2])        #report p-value of linear hypothesis test that Beta_0 and Beta_1 are equals to zero and one, resp.
    quantile.reg.stats[(k + 5), 1] <- as.numeric(lin.hyp[2, "Pr(>Chisq)"])
    k <- k + 6
  }
  
  ######quantile reg stats return (Table2Es)
  # return(quantile.reg.stats)
  # print("ok")
  ###### percentage of violation return (Table3Es)
  es.violations.mean <- rowMeans(es.violations, na.rm = T)
  # print("ok")
  es.violation.stat <- sum(vec.ret > es.violations.mean) / length(vec.ret)
  # return(es.violation.stat)

  vas.y <- VcvMqr(vec.ret, var.t, b.hat, u)                                         # Asymp. covariance matrix estimate of the MQR parameters
            
  b.hat.vec <- matrix(data = b.hat, nrow = 2 * p, ncol = 1)                         # Vector of parameter estimates
  
  # print(b.hat.vec)

#=============================#
#==      The backtests      ==#
#=============================#
   
#== J1 test statistic (Joint backtest with 1 constraint) ==#

  r.j.1 <- t(matrix(data = 1, nrow = 2 * p, ncol = 1))                      # Constraint of the J1 joint backtest
  q.j.1 <- p                                                                # Expected value of the coefficients under the null
  j.1 <- t(r.j.1 %*% b.hat.vec - q.j.1) %*% ginv(r.j.1 %*% vas.y %*% t(r.j.1)) %*% (r.j.1 %*% b.hat.vec - q.j.1)
  # print(j.1)

#== J2 test statistic (Joint backtest with 2 constraints) ==#

  r.j.2 <- matrix(data = diag(2), nrow = 2, ncol = 2 * p)                     # Constraints of the J2 joint backtest
  q.j.2 <- matrix(data = c(0, p), nrow = 2, ncol = 1)                        # Expected value of the coefficients under the null
  j.2 <- t(r.j.2 %*% b.hat.vec - q.j.2) %*% ginv(r.j.2 %*% vas.y %*% t(r.j.2)) %*% (r.j.2 %*% b.hat.vec - q.j.2)   # J2 test statistic
  # print(j.2)
  
##== I test statistic (Intercept backtest) ==#
  
  r.i <- matrix(data = c(1, 0), nrow = 1, ncol = 2 * p)                         # Constraint of the intercept backtest
  q.i <- 0                                                                   # Expected value of the coefficients under the null
  big.i <- t(r.i %*% b.hat.vec) %*% ginv(r.i %*% vas.y %*% t(r.i)) %*% (r.i %*% b.hat.vec)   # I test statistic
  # print(big.i)
  
#== S test statistic (Slope backtest) ==#

  r.s <- matrix(data = c(0, 1), nrow = 1, ncol = 2 * p)                         # Constraint of the slope backtest
  q.s <- p                                                                   # Expected value of the coefficients under the null
  big.s <- t(r.s %*% b.hat.vec - q.s) %*% ginv(r.s %*% vas.y %*% t(r.s)) %*% (r.s %*% b.hat.vec - q.s)  # S test statistic
  # print(big.s)
  
  #==========================================================#
  #== Individual inference by coefficients and risk levels ==#
  #==========================================================#
    
  b.hat.vec.2 <- matrix(data = t(b.hat.vec) - r.s, nrow = 2, ncol = p)      # Centered parameters under their expected values of the null 
  # print(b.hat.vec.2)
  vas.y.ind <- matrix(data = diag(vas.y), nrow = 2, ncol = p)               # Asymp. variance of the parameters
  # print(vas.y.ind)
  ind.test <- (b.hat.vec.2^2) / vas.y.ind                                   # Individual test statistics
  
  # print("ind.test is: ")
  # print(ind.test)
  # return(ind.test)
  
  #==================================================#
  #== Joint inference (b0 and b1) by risk levels   ==#
  #==================================================#
    
  joint.test <- matrix(data = NA, nrow = 1, ncol = p)
  
  for(k in 1:p){
    
    b.hat.2.c <- b.hat[, k] - matrix(data = c(0, 1), nrow = 2, ncol = 1)    # Centered parameters under their expected values of the null
    cov.2.c <- vas.y[(2 * k - 1) : (2 * k), (2 * k - 1) : (2 * k)]          # Asymp. covariance matrix of the joint parameters
    joint.test[1, k] <- t(b.hat.2.c) %*% ginv(cov.2.c) %*% b.hat.2.c         #Joint test statistics
  }
  
  # print("joint.test is: ")
  # print(joint.test)
  
  # return(joint.test)
  #===========================================#
  #== Implementation of the pairs bootstrap ==#
  #===========================================#
  
  # big.b <- 1000                                                              #Number of bootstrapped sample
                                     
  j.1.boot <- matrix(data = NA, nrow = big.b, ncol = 1)                        #Initialization of J1 and J2 bootstrapped statistics 
  j.2.boot <- matrix(data = NA, nrow = big.b, ncol = 1)
  
  i.boot <- matrix(data = NA, nrow = big.b, ncol = 1)                          #Initialization of I and S bootstrapped statistics
  s.boot <- matrix(data = NA, nrow = big.b, ncol = 1)
  
  ind.test.boot <- array(data = NA, dim = c(big.b, p, 2))                      #Initialization of ind. and joint bootstrapped statistics          
  joint.test.boot <- matrix(data = NA, nrow = big.b, ncol = p)
  
  set.seed(123456789)                                                          #Seed to make the results reproducible
                                       
  my.index <- floor(n * matrix(runif(n * big.b), nrow = n)) + 1                #Random draws for resampling the data
  
  # Generate m×n matrix of discrete uniform random integers between 1 and k
  
  print("starting bootstrap: ")
  
  # parfor b=1:B
  for (b in 1:big.b){                                                       #Loop of the bth bootstrap iteration (try foreach package in R, allows for parallelization)
    
    if(b %% 500 == 0){
      print(b)
    }
    
  l.boot <- vec.ret[my.index[, b]]                                          # Resampled losses
  var.t.boot <- var.t[my.index[, b], ]                                      # Resampled VaRs
  # print(head(var.t.boot))
  
  var.t.boot <- matrix(data = var.t.boot, ncol = p)
  # print(head(var.t.boot))
  # return()
  
  b.boot <- matrix(data = NA, nrow = 2, ncol = p)                           # Initialization of the bootstrap MQR parameters    
  
  
  for(j in 1:p){
    temp.rq.reg <- rq(l.boot~var.t.boot[, j], tau = u[j, 1])
    b.boot[, j] <- as.numeric(coefficients(temp.rq.reg))   # MQR bootstrap parameter estimates
  }
  
  vas.y.boot <- VcvMqr(l.boot, var.t.boot, b.boot, u)                          # Asymp. covariance matrix estimate      
  
  b.boot.vec <- matrix(data = b.boot, nrow = 2 * p, ncol = 1)                  # Vector of parameter estimates
  
  # J1 bootstrap test statistic
  j.1.boot[b, 1] <- t(r.j.1 %*% b.boot.vec - r.j.1 %*% b.hat.vec) %*% ginv(r.j.1 %*% vas.y.boot %*% t(r.j.1)) %*% (r.j.1 %*% b.boot.vec - r.j.1 %*% b.hat.vec)
  # J2 bootstrap test statistic
  j.2.boot[b, 1] <- t(r.j.2 %*% b.boot.vec - r.j.2 %*% b.hat.vec) %*% ginv(r.j.2 %*% vas.y.boot %*% t(r.j.2)) %*% (r.j.2 %*% b.boot.vec - r.j.2 %*% b.hat.vec)
  # I bootstrap test statistic
  
  ##### DO NOT COMPUTE ONLY FOR PAPER REVISION TABLE 8
  # i.boot[b, 1] <- t(r.i %*% b.boot.vec - r.i %*% b.hat.vec) %*% ginv(r.i %*% vas.y.boot %*% t(r.i)) %*% (r.i %*% b.boot.vec - r.i %*% b.hat.vec)
  # S bootstrap test statistic 
  ##### DO NOT COMPUTE ONLY FOR PAPER REVISION TABLE 8
  # s.boot[b, 1] <- t(r.s %*% b.boot.vec - r.s %*% b.hat.vec) %*% ginv(r.s %*% vas.y.boot %*% t(r.s)) %*% (r.s %*% b.boot.vec - r.s %*% b.hat.vec)
  b.boot.vec.2 <- matrix(data = t(b.boot.vec) - r.s, nrow = 2, ncol = p)       # Centered bootsrap parameters under their expected values of the null 
  vas.y.boot.ind <- matrix(data = diag(vas.y.boot), nrow = 2, ncol = p)        # Individual variances of the bootstrap parameters
  ind.test.boot[b, , ] <- (b.boot.vec.2 - b.hat.vec.2)^2 / vas.y.boot.ind      # Individual bootstrap test statistics

   for(k in 1:p){
     
     b.hat.2.c <- b.hat[, k] - matrix(data = c(0, 1), nrow = 2, ncol = 1)      # Centered parameters under their expected values of the null
     b.hat.2.c.boot <- b.boot[, k] - matrix(data = c(0, 1), nrow = 2, ncol = 1) # Centered bootstrap parameters under their expected values of the null                                
     cov.2.c.boot <- vas.y.boot[(2 * k - 1) : (2 * k), (2 * k - 1) : (2 * k)]    #Asymp. covariance matrix of the joint parameters
     joint.test.boot[b, k] <- t(b.hat.2.c.boot - b.hat.2.c) %*% ginv(cov.2.c.boot) %*% (b.hat.2.c.boot - b.hat.2.c)   # Joint test statistics for a given risk level
      
   } #end of loop k
}   # End of loop b
  
  ##### DO NOT COMPUTE ONLY FOR PAPER REVISION TABLE 8
  # p.val.ind.test.b0 <- colMeans(ind.test.boot[, , 1] > matrix(data = ind.test[1, ], nrow = big.b, ncol = p, byrow = T)) 
  # p.val.ind.test.b1 <- colMeans(ind.test.boot[, , 2] > matrix(data = ind.test[2, ], nrow = big.b, ncol = p, byrow = T))
  # p.val.joint.test <- colMeans(joint.test.boot > matrix(data = joint.test, nrow = big.b, ncol = p, byrow = T))
  
  #===========================================#
  #== Computation of adjusted ES forecasts  ==#
  #===========================================#
  # Adjusted ES forecasts
  q.t.out <- matrix(data = b.hat[1, ], nrow = n, ncol = p, byrow = T) + matrix(data = b.hat[2, ], nrow = n, ncol = p, byrow = T) * var.t

  p.val.j.1 <- mean(j.1.boot > as.numeric(j.1), na.rm = T)
  p.val.j.2 <- mean(j.2.boot > as.numeric(j.2), na.rm = T)
  # p.val.i <- mean(i.boot > as.numeric(big.i), na.rm = T)
  # p.val.s <- mean(s.boot > as.numeric(big.s), na.rm = T)

  return(c(p.val.j.1, p.val.j.2))
  
  ##return bootstrap p values (Table 1 ES)
  return(c(p.val.j.1, p.val.j.2, p.val.i, p.val.s))
}

#p-values from predictive accuracy
Table1Es <- function(big.b = 1000
                     , list.forecasts = list.df.out.1
                     , df.ret.out = df.ret.out){
  
  library(stargazer)
  library(qrmtools)
  library(quantreg)
  library(MASS)
  library(car)
  
  #4 risk levels  and 4 parameters * five models
  df.res <- data.frame(matrix(data = NA, nrow = 1 * 2, ncol = 12))
  
  # vec.p = c(1, 2, 4, 6)
  vec.p = 4
  
  for(j in 1:12){
    k = 1
    # for(i in 1:4){
    for(i in 1:1){

      df.res[k:(k + 3), j] <- ExpectedShortfall(big.b = big.b
                              , vec.forecasts = as.numeric(unlist(list.forecasts[[j]]$df.forecast))
                              , vec.ret = as.numeric(unlist(df.ret.out))
                              , dof = 8
                              , p = vec.p[i])

      k <- k + 4
      stargazer(df.res, summary = FALSE, digits = 2, digits.extra = 2)
    }
  }

  stargazer(df.res, summary = FALSE, digits = 2, digits.extra = 2)
  
  return(df.res)
}

##alpha, beta and joint test for each level tau of quantile regressions
Table2Es <- function(big.b = 1000
                    , list.forecasts = list.df.out
                    , df.ret.out = df.ret.out){
  
  library(stargazer)
  library(qrmtools)
  library(quantreg)
  library(MASS)
  library(car)
  
  p <- 6
  
  df.res <- data.frame(matrix(data = NA, nrow = p * 6, ncol =  12))
  
  # k <- 1
  for(j in 1:12){
    
    print(paste0("model estimated is number: ", j))
    
    df.res[, j] <-  ExpectedShortfall(big.b = big.b
                      , vec.forecasts = as.numeric(unlist(list.forecasts[[j]]$df.forecast))
                      , vec.ret = as.numeric(unlist(df.ret.out))
                      , dof = 8
                      , p = p)
    
    # k <- k + 5
  }

  stargazer(df.res, summary = FALSE, digits = 2, digits.extra = 2)
  return(df.res)
}


# compute % of violation for each contract / model.
Table3Es <- function(big.b = 1000
                     , list.forecasts = list.df.out
                     , df.ret.out = df.ret.out){
  
  library(stargazer)
  library(qrmtools)
  library(quantreg)
  library(MASS)
  library(car)
  
  p = 1
  #percent of violation 
  df.res <- data.frame(matrix(data = NA, nrow = 9 + 2, ncol =  12))
  
  rownames(df.res) <- c("C", "S", "W", "CL", "HO", "NG", "GC", "HG", "SI", "Average", "# first")
  print("ok")
  for(j in 1:12){
    for(i in 1:9){
      
      df.res[i, j] <- ExpectedShortfall(big.b = big.b
                              , vec.forecasts = list.forecasts[[j]]$df.forecast[, i]
                              , vec.ret = as.numeric(df.ret.out[, i])
                              , dof = 8
                              , p = p)
    }
  }
  
  print("ok")
  df.res[10, ] <- colMeans(df.res[1:9, ])
  print(apply(df.res, 2, which.min))
  
  df.res <- df.res * 100 / 2
  
  stargazer(df.res, summary = FALSE, digits = 2, digits.extra = 2)
  return(df.res)
}

# --------------------------------------------------------------------------
#Compute the covariance matrix of the multi-quantile regression parameters (Leymari)
  
VcvMqr <- function(df.y, var.t, b.hat, tau){
    
  df.y <- data.frame(df.y)
  n.df <- dim(df.y)[1]
  p.df <- dim(df.y)[2]
    
  p <- dim(tau)[1]
  
  eta <- matrix(data = NA, nrow = n.df, ncol = 2 * p)
  c.t <- n.df^(-1 / 7)
  q.t <- array(data = NA, dim = c(2 * p, 2 * p,  n.df))
  
  for (i in 1:n.df){
    
    eta.t.j <- matrix(data = NA, nrow = 2 * p, ncol = p)
    q.t.j <- array(data = NA, dim = c(2 * p, 2 * p, p))
    
    nabla.t <- rbind(matrix(data = 1, nrow = 1, ncol = p), var.t[i, ])

    for(j in 1:p){
    
      nabla.t.j <- matrix(data = 0, nrow = 2, ncol = p)
      nabla.t.j[, j] <- nabla.t[, j]
      nabla.t.j <- matrix(data = nabla.t.j, nrow = 2 * p, ncol = 1, byrow = TRUE)
      eps.j.t <- df.y[i, 1] - matrix(data = c(1, var.t[i, j]), nrow = 1, ncol = 2) %*% b.hat[, j]
      eta.t.j[, j] <- nabla.t.j * (tau[j, 1] - as.numeric(eps.j.t <= 0))
      q.t.j[, , j] <- as.numeric(abs(eps.j.t) <= c.t) * nabla.t.j %*% t(nabla.t.j)
    }
      q.t[, , i] <- rowSums(q.t.j, dims = 2)
      eta[i, ] <- rowSums(eta.t.j)
  }
  
  big.v <- (t(eta) %*% eta) / n.df
  big.q <- rowSums(q.t, dims = 2) / (2 * c.t * n.df)
  vas.y <- (ginv(big.q) %*% big.v %*% ginv(big.q)) / n.df
  
  return(vas.y)
}