#takes as argument list.df.in or list.df.out, list of lists of forecast, realized, and errors of RV models
ModelConfidenceSet <- function(list.df){
  
  library(MCS)
  library(stargazer)
  #get length of the loss:
  n.loss <- dim(list.df[[1]]$df.errors)[1]
  
  df.mcs <- data.frame(matrix(data = NA, nrow = 3, ncol = 12))
  
  df.mse <- matrix(data = NA, nrow = n.loss * 9, ncol = 12)
  df.mae <- matrix(data = NA, nrow = n.loss * 9, ncol = 12)
  df.qlike <- matrix(data = NA, nrow = n.loss * 9, ncol = 12)
  
  # compute the losses
  for(j in 1:12){
    
    df.mse[, j] <- LossVol(realized = as.numeric(unlist(list.df[[j]]$df.real)), evaluated = as.numeric(unlist(list.df[[j]]$df.forecast)), which = "SE1")
    df.mae[, j] <- LossVol(realized = as.numeric(unlist(list.df[[j]]$df.real)), evaluated = as.numeric(unlist(list.df[[j]]$df.forecast)), which = "AE1")
    df.qlike[, j] <- LossVol(realized = as.numeric(unlist(list.df[[j]]$df.real)), evaluated = as.numeric(unlist(list.df[[j]]$df.forecast)), which = "QLIKE")
    
    df.mcs[1, j] <- round(mean(df.mse[, j]), 4)
    df.mcs[2, j] <- round(mean(df.mae[, j]), 4)
    df.mcs[3, j] <- round(mean(df.qlike[, j]), 4)
    
  }
  
  # return(df.mse)
   
  mcs.mse <- MCSprocedure(df.mse, alpha = 0.1, B = 1000)
  print(mcs.mse)
  
  # return(mcs.mse)
  mcs.mae <- MCSprocedure(df.mae, alpha = 0.1, B = 1000)
  # print(mcs.mae)
  # # 
  mcs.qlike <- MCSprocedure(df.qlike, alpha = 0.1, B = 1000)
  # print(mcs.qlike)
  
  print("ok")
  
  print(mcs.mse)
  print(mcs.mae)
  print(mcs.qlike)


  #mse comparison
  if(dim(mcs.mse@show)[1] > 0){
    for(j in 1:dim(mcs.mse@show)[1]){
      model.names <- rownames(mcs.mse@show)
      my.ind <- as.numeric(substring(model.names[j], 7))
      df.mcs[1, my.ind] <- RankingStars(df.mcs[1, my.ind], mcs.mse@show[j, "Rank_M"])
    }
  }
  
  #mae comparison
  if(dim(mcs.mae@show)[1] > 0){
    for(j in 1:dim(mcs.mae@show)[1]){
      model.names <- rownames(mcs.mae@show)
      my.ind <- as.numeric(substring(model.names[j], 7))
      df.mcs[2, my.ind] <- RankingStars(df.mcs[2, my.ind], mcs.mae@show[j, "Rank_M"])
    }
  }
  
  #qlike comparison
  if(dim(mcs.qlike@show)[1] > 0){
    for(j in 1:dim(mcs.qlike@show)[1]){
      model.names <- rownames(mcs.qlike@show)
      my.ind <- as.numeric(substring(model.names[j], 7))
      df.mcs[3, my.ind] <- RankingStars(df.mcs[3, my.ind], mcs.qlike@show[j, "Rank_M"])
    }
  }
  
  stargazer(df.mcs, summary = FALSE, digits = 4, digits.extra = 2)
  
  return(df.mcs)
}

RankingStars <- function(my.val, my.rank){
  
  return(paste0(as.character(my.val), "^", as.character(my.rank)))
}