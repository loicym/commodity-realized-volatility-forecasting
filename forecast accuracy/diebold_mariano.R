##takes as argument list.df.in or list.df.out, list of lists of forecast, realized, and errors of RV models
DieboldMariano <- function(list.df, h = c(1, 5, 22)){
  
  library(forecast)
  library(systemfit)
  library(gtools)
  
  df.dm.test.mae <- matrix(data = NA, ncol = 12, nrow = 12)
  df.dm.test.mse <- matrix(data = NA, ncol = 12, nrow = 12)
  
  names.models <- c("EV", "HAR", "HAR-EV", "HEXP", "HEXP-EV", "HAR-Q", "HAR-Q-EV", "HAR-TV", "HAR-TV-EV", "HAR-TV-Q", "HAR-TV-Q-EV", "RiskMetrics")
  
  rownames(df.dm.test.mae) <- names.models
  colnames(df.dm.test.mae) <- names.models
  
  rownames(df.dm.test.mse) <- names.models
  colnames(df.dm.test.mse) <- names.models
  
  
  # temp.dm.test.mse <- dm.test(as.numeric(unlist(list.df[[1]]$df.errors))
  #                             , as.numeric(unlist(list.df[[2]]$df.errors))
  #                             , alternative = "two.sided" #c("two.sided") #alternative="two.sided", alternative = less, alternative = greater
  #                             , h = h
  #                             , power = 2)
  
  # print(temp.dm.test.mse)
  # return()
  
  for(i in 1:12){
    for(j in 1:12){
      if(j < i){
        
        # temp.dm.test.mae <- dm.test(as.numeric(unlist(list.df[[j]]$df.errors))
        #                             , as.numeric(unlist(list.df[[i]]))
        #                             , alternative = c("two.sided") #alternative="two.sided", alternative = less, alternative = greater
        #                             , h = h
        #                             , power = 1)
        # 
        # df.dm.test.mae[i, j] <- paste0("&$", format(round(temp.dm.test.mae$statistic[1], 2), nsmall = 2), "^{", stars.pval(temp.dm.test.mae$p.value[1]), "}$")
        
        temp.dm.test.mse <- dm.test(as.numeric(unlist(list.df[[j]]$df.errors))
                                    , as.numeric(unlist(list.df[[i]]$df.errors))
                                    , alternative = c("two.sided") #alternative="two.sided", alternative = less, alternative = greater
                                    , h = h
                                    , power = 2)
        
        df.dm.test.mse[i, j] <- paste0("&$", format(round(temp.dm.test.mse$statistic[1], 2), nsmall = 2), "^{", stars.pval(temp.dm.test.mse$p.value[1]), "}$")
      }
    }
  }
  
  # df.dm.test.mae[is.na(df.dm.test.mae)] <- "&"
  df.dm.test.mse[is.na(df.dm.test.mse)] <- "&"
  
  # print(df.dm.test.mae)
  print(df.dm.test.mse)
  
  # stargazer(df.dm.test.mae, summary = FALSE, digits = 2, digits.extra = 2)
  # stargazer(df.dm.test.mse, summary = FALSE, digits = 2, digits.extra = 2)
}