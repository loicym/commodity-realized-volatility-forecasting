#set of functions to compute all volatility factors needed
#1: in built
#2: in built, maybe alternative with dummy for each sets of days.... but should
#be captured by the linear days. / square root of days to maturity?
#3: dummies coded for month

#3 dummies coded for month. function returns a df that has an additional 12 dummies
#for each month (based on the trading day of CL)
monthly_dummies = function(df)
{
  uX = dim(df)[1]
  dJan = rep(0,uX)
  dFeb = rep(0,uX)
  dMar = rep(0,uX)
  dApr = rep(0,uX)
  dMay = rep(0,uX)
  dJun = rep(0,uX)
  dJul = rep(0,uX)
  dAug = rep(0,uX)
  dSep = rep(0,uX)
  dOct = rep(0,uX)
  dNov = rep(0,uX)
  dDec = rep(0,uX)
  
  myDate = as.POSIXct(df$tradingDay1CL,origin="1970-01-01")
  myMonth = format(myDate,format="%b")
  
  dJan[myMonth=="Jan"] <- 1
  dFeb[myMonth=="Feb"] <- 1
  dMar[myMonth=="Mar"] <- 1
  dApr[myMonth=="Apr"] <- 1
  dMay[myMonth=="May"] <- 1
  dJun[myMonth=="Jun"] <- 1
  dJul[myMonth=="Jul"] <- 1
  dAug[myMonth=="Aug"] <- 1
  dSep[myMonth=="Sep"] <- 1
  dOct[myMonth=="Oct"] <- 1
  dNov[myMonth=="Nov"] <- 1
  dDec[myMonth=="Dec"] <- 1
  
  dummyDf = data.frame(cbind(dJan,dFeb,dMar,dApr,dMay,dJun,dJul,dAug,dSep,dOct,dNov,dDec))
  # df = data.frame(cbind(df,dJan,dFeb,dMar,dApr,dMay,dJun,dJul,dAug,dSep,dOct,dNov,dDec))
  return(dummyDf)
  # return(df)
}

