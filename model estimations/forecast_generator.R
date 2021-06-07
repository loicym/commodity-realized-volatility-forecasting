##Computes all forecasts, forecast errors, and realized volatility of the 12 models.
ForecastGen <- function(df.rv = list.5.min$rvar
                                 , df.sl = list.5.min$slope
                                 , df.time.matu = list.5.min$time.matu
                                 , mat.calendar.dum = list.5.min$month.dummies
                                 , out.sample.mode = FALSE
                                 , training.sample.length = 1500
                                 , n.ahead = c(1, 5, 22, 125, 250)
                                 , current.list){
  

  list.forecast <- list()
  
  print("start EV")
  list.forecast[[1]] <- Ev(df.rv = df.rv
                         , df.sl = df.sl
                         , df.time.matu = df.time.matu
                         , mat.calendar.dum = mat.calendar.dum
                         , out.sample.mode = out.sample.mode
                         , training.sample.length = training.sample.length
                         , n.ahead = n.ahead)
  
  
  print("start HAR")
  list.forecast[[2]] <- Har(df.rv = df.rv
                          , out.sample.mode = out.sample.mode
                          , training.sample.length = training.sample.length
                          , n.ahead = n.ahead)
  
  print("start HAR-EV")
  list.forecast[[3]] <- HarEv(df.rv = df.rv
                            , df.sl = df.sl
                            , df.time.matu = df.time.matu
                            , mat.calendar.dum = mat.calendar.dum
                            , out.sample.mode = out.sample.mode
                            , training.sample.length = training.sample.length
                            , n.ahead = n.ahead)
  
  print("start HEXP")
  list.forecast[[4]] <- HarExp(df.rv = df.rv
                             , out.sample.mode = out.sample.mode
                             , training.sample.length = training.sample.length
                             , n.ahead = n.ahead)
  
  print("start HEXP-EV")
  list.forecast[[5]] <- HarExpEv(df.rv = df.rv
                               , df.sl = df.sl
                               , df.time.matu = df.time.matu
                               , mat.calendar.dum = mat.calendar.dum
                               , out.sample.mode = out.sample.mode
                               , training.sample.length = training.sample.length
                               , n.ahead = n.ahead)
  
  print("start HAR-Q")
  list.forecast[[6]] <- HarQ(df.rv = df.rv
                             , out.sample.mode = out.sample.mode
                             , training.sample.length = training.sample.length
                             , n.ahead = n.ahead)
  
  print("start HAR-Q-EV")
  list.forecast[[7]] <- HarQEv(df.rv = df.rv
                               , df.sl = df.sl
                               , df.time.matu = df.time.matu
                               , mat.calendar.dum = mat.calendar.dum
                               , out.sample.mode = out.sample.mode
                               , training.sample.length = training.sample.length
                               , n.ahead = n.ahead)
  
  print("start HAR-TV")
  list.forecast[[8]] <- HarTv(df.rv = df.rv
                           , out.sample.mode = out.sample.mode
                           , training.sample.length = training.sample.length
                           , n.ahead = n.ahead)
  
  print("start HAR-TV-EV")
  list.forecast[[9]] <- HarTvEv(df.rv = df.rv
                               , df.sl = df.sl
                               , df.time.matu = df.time.matu
                               , mat.calendar.dum = mat.calendar.dum
                               , out.sample.mode = out.sample.mode
                               , training.sample.length = training.sample.length
                               , n.ahead = n.ahead)
  
  print("start HAR-TV-Q")
  list.forecast[[10]] <- HarTvQ(df.rv = df.rv
                           , out.sample.mode = out.sample.mode
                           , training.sample.length = training.sample.length
                           , n.ahead = n.ahead)
  
  print("start HAR-TV-Q-EV")
  list.forecast[[11]] <- HarTvQEv(df.rv = df.rv
                               , df.sl = df.sl
                               , df.time.matu = df.time.matu
                               , mat.calendar.dum = mat.calendar.dum
                               , out.sample.mode = out.sample.mode
                               , training.sample.length = training.sample.length
                               , n.ahead = n.ahead)
  
  print("start RiskMetrics")
  list.forecast[[12]] <- RiskMetrics(df.rv = df.rv
                                , out.sample.mode = out.sample.mode
                                , training.sample.length = training.sample.length
                                , n.ahead = n.ahead)
  
  return(list.forecast)
}