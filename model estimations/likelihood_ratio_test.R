##equivalent for systems of equations
LrTestJoint <- function(mod.restricted, mod.unrestricted){
  # library(nonnest2)
  library(gtools)
  library(systemfit)
  
  my.lrtest <- lrtest(mod.restricted, mod.unrestricted)
  
  print(my.lrtest)
}

##compute myself log likelihood ratio test from errors (see systemfit package methodo)
myLrTest <- function(mod1 = mod.har.tv
                     , mod2 = mod.har.tv.ev){
  
  df.errors1 <- data.matrix(residuals(mod1))
  df.cov.errors1 <- cov(df.errors1)
  
  print(head(df.errors1))
  
  df.errors2 <- data.matrix(residuals(mod2))
  df.cov.errors2 <- cov(df.errors2)
  
  big.t <- dim(df.errors1)[1]
  
  print(log(det(df.cov.errors1)))
  
  print(log(det(df.cov.errors2)))
  
  my.lr.test <- big.t * (log(det(df.cov.errors1)) - log(det(df.cov.errors2)))
  
  print(my.lr.test)
}



##compute myself the log-likelihood of a system from errors (source = vars package), 
#see: https://www.rdocumentation.org/packages/vars/versions/1.5-3/topics/logLik
myLogLik <- function(mod = mod.har){
  
  library(matrixcalc)
  df.errors <- data.matrix(residuals(mod))
  df.cov.errors <- cov(df.errors)
  
  # df.coeffs <- data.matrix(mod$coefficients)
  # df.cov.coeffs <- data.matrix(mod$coefCov)

  big.t <- dim(df.errors)[1]
  log.lik.t <- rep(0, big.t) 
  big.m <- 9
  
  for(i in 1:big.t){
    
    log.lik.t[i] <- - (big.m / 2) * log(2 * pi) - (1 / 2) * log(det(df.cov.errors)) - (1 / 2) * t(df.errors[i, ]) %*% solve(df.cov.errors) %*% (df.errors[i, ])
  }
  
  return(sum(log.lik.t))
  
  
  big.w <- (1 / big.t) * 
  # big.t <- 
  # big.k <- dim(df.errors)[2]
  big.k <- 27
  
  my.log.lik <- - ((big.k * big.t) / 2) * log(2 * pi) - (big.t / 2) * log(det(df.cov.errors)) - (1 / 2) * sum(diag(df.errors %*% solve(df.cov.errors) %*% t(df.errors)))
  # my.log.lik <- - ((big.k * big.t) / 2) * log(2 * pi) - (big.t / 2) * log(one.norm(df.cov.coeffs)) - (1 / 2) * matrix.trace(t(df.coeffs) %*% solve(df.cov.coeffs) %*% df.coeffs)
  print(my.log.lik)
}


