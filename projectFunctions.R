library(AppliedPredictiveModeling)
library(caret)

set.seed(1337)

cleanse <- function(df){
  #remove window = 'yes' rows
  df <- df[df$new_window =='no',]
  #remove the date / time column and new_window column
  df <- df[,-5:-6]
  # remove all columns which are all na
  df <- (df[ , ! apply( df , 2 , function(x) all(is.na(x)) ) ])
  return (df)
}

# function to write files
pml_write_files <- function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}