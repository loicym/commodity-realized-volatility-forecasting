#function to correct the rv variable of the data frame and replace it by the real vol.
correct_vol = function(df)
{
  tickers = c("C","CL","GC","HG","HO","NG","S","SI","W")
  
  n = length(tickers)
  
  dev.new()
  plot(df$rv1C,t='l')
  lines(df$rv2C,col='red')
  for(i in 1:n)
  {
    tField = paste0("rv1",tickers[i])
    df[,tField] <- df[,tField]^0.5
    
    tField = paste0("rv2",tickers[i])
    df[,tField] <- df[,tField]^0.5
    
    tField = paste0("dayRv1",tickers[i])
    df[,tField] <- df[,tField]^0.5
    
    tField = paste0("weekRv1",tickers[i])
    df[,tField] <- df[,tField]^0.5
    
    tField = paste0("monthRv1",tickers[i])
    df[,tField] <- df[,tField]^0.5
  }
  
  tField = "dayRvAgriculture"
  df[,tField] <- df[,tField]^0.5
  
  tField = "dayRvEnergy"
  df[,tField] <- df[,tField]^0.5
  
  tField = "dayRvMetal"
  df[,tField] <- df[,tField]^0.5
  
  tField = "weekRvAgriculture"
  df[,tField] <- df[,tField]^0.5
  
  tField = "weekRvEnergy"
  df[,tField] <- df[,tField]^0.5
  
  tField = "weekRvMetal"
  df[,tField] <- df[,tField]^0.5
  
  tField = "monthRvAgriculture"
  df[,tField] <- df[,tField]^0.5
  
  tField = "monthRvEnergy"
  df[,tField] <- df[,tField]^0.5
  
  tField = "monthRvMetal"
  df[,tField] <- df[,tField]^0.5
  
  
  print(head(df))
  return(df)
}