cleaner.df <- function(df){
  
  n.df <- dim(df)[1]
  p.df <- dim(df)[2]
  
  for(j in 1:p.df){
    for(i in 2:n.df){
      if(is.na(df[i, j]) || abs(df[i, j]) == Inf){
        df[i, j] <- df[(i - 1), j]
      }
    }
  }
  return(df)
}

cleaner.vec <- function(vec){
  
  n <- length(vec)

  for(i in 2:n){
    if(is.na(vec[i]) || abs(vec[i]) == Inf){
      vec[i] <- vec[(i - 1)]
    }
  }
  return(vec)
}