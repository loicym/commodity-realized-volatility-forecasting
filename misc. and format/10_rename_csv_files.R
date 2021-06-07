RenameFile <- function(end.path)
{
  my.path <- paste0("C:/Users/loicy/Dropbox/unine/loic/thesis/R/RProj2 - rv/data/1 min/", end.path)
  
  setwd(my.path)
  my.files <- list.files(my.path)
  print(my.files)
  print(length(my.files))
  
  # my.list <- FileReader(my.path, my.files)
  
  n.list <- length(my.files)
  print("ok")
  # return()
  for(i in 1:n.list){
    
    name.file <- my.files[[i]]
    
    ticker.name <- substring(name.file, 1, 2)
    year.name <- substring(name.file, 4, 5)
    month.name <- substring(name.file, 3, 3)
    
    print(name.file)
    print(ticker.name)
    print(year.name)
    print(month.name)
    
    

    new.name <- paste0(ticker.name, year.name, month.name, ".csv")
    
    print(new.name)
    
    file.rename(from = name.file, to = new.name)
    # return()
    # my.list[[i]] <- FileFormater(my.list[[i]])
    # print(head(my.list[[i]]))
  }

}