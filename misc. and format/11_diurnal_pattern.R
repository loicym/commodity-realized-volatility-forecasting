
#extract diurnal patterns information of e.g., volume.
DiurnalPattern <- function(mat.min){
  
  datetime.stamp <- mat.min[, 1]
  time.stamp <- format(as.POSIXct(datetime.stamp, origin = "1970-01-01"), format = "%H:%M:%S")
  print(head(time.stamp))
  
  level.time.stamp <- levels(as.factor(time.stamp))
  
  df.time <- data.frame(cbind(time.stamp, data.frame(mat.min)))
  print(head(df.time))
  
  
  tt <- aggregate(df.time$volume1.GC, by = list(time.stamp = df.time$time.stamp), FUN = mean)
  
  print(tt)
  
  dev.new()
  plot(tt$x)
  return()
  
  for(j in 1:9)
  {
    
    
    
    
    
    
  }
  
  
  
  
  
  
}