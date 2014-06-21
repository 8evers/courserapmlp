#remember to set working directory

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

#load data
sourceSample <- read.csv("pml-training.csv", header=TRUE, na.strings=c("","NA"))
sourceFinalSet <- read.csv("pml-testing.csv", header=TRUE, na.strings=c("","NA"))

#clean data
cleanSample <- cleanse(sourceSample)
cleanFinalSet <- cleanse(sourceFinalSet)

#put both train and test together for expansion (don't want to expand classe)
fullSet = rbind(cleanFinalSet[,-58], cleanSample[,-58])
#remove X and Expand factor variables
expandedVariables = data.frame(model.matrix(X ~ . - 1, data = fullSet)) 
#splitt the data back into train and test
expandedSample = expandedVariables[-1:-20,]
expandedFinalSet = expandedVariables[1:20,]
expandedSample["classe"] = cleanSample$classe #restore classe
expandedFinalSet["problem_id"] = cleanFinalSet$problem_id #restore problem_id

#split data for cross validation this is the Raw training set which all other variables are created from.
inTest <- createDataPartition(expandedSample$classe, p = 0.2)[[1]]
trainCrossValRaw <- expandedSample[-inTest,]
testRaw <- expandedSample[inTest,]
inCrossVal <- createDataPartition(trainCrossValRaw$classe, p = 0.25)[[1]]
trainRaw <- trainCrossValRaw[-inCrossVal,]
crossValRaw <- trainCrossValRaw[inCrossVal,]
finalRaw = expandedFinalSet

#Round1
preproc1 <- preProcess(trainRaw[,-62], method="pca", thresh=.99)
trainPCA <- predict(preproc1, trainRaw[,-62])
crossValPCA <- predict(preproc1, crossValRaw[,-62])
finalPCA <- predict(preproc1, finalRaw[,-62])
fitPCA <- train(trainRaw$classe~.,data=trainPCA, method="knn")
predictPCA <- predict(fitPCA, crossValPCA)
predictFinaLPCA <- predict(fitPCA, finalPCA)
confusionMatrix(crossValRaw$classe, predictPCA)

#Round2
minTs <- min(trainRaw$raw_timestamp_part_1)
trainCols <- trainRaw[,1:6]
crossValCols <- crossValRaw[,1:6]
testCols = testRaw[,1:6]
finalCols = finalRaw[,1:6]
trainCols["timestamp"] <- as.double(trainRaw$raw_timestamp_part_1 - minTs) + as.double(trainRaw$raw_timestamp_part_2) / 1000000.0
crossValCols["timestamp"] <- as.double(crossValRaw$raw_timestamp_part_1 - minTs) + as.double(crossValRaw$raw_timestamp_part_2) / 1000000.0
testCols["timestamp"] <- as.double(testRaw$raw_timestamp_part_1 - minTs) + as.double(testRaw$raw_timestamp_part_2) / 1000000.0
finalCols["timestamp"] <- as.double(expandedFinalSet$raw_timestamp_part_1 - minTs) + as.double(expandedFinalSet$raw_timestamp_part_2) / 1000000.0

fitCols <- train(trainRaw$classe~.,data=trainCols, method="knn")
predictCols <- predict(fitCols, crossValCols)
confusionMatrix(crossValRaw$classe, predictCols)
predictFinalCols <- predict(fitCols, finalCols)

#Round3
trainNoTime <- trainRaw[,c(-1:-9,-63)]
crossValNoTime <- crossValRaw[,c(-1:-9,-63)]
testNoTime <- testRaw[,c(-1:-9,-63)]

svmGrid <-  expand.grid(C=c(128), sigma=c(0.03))
fitNoTime = train(trainRaw$classe~.,data=trainNoTime, verbose = TRUE, method="svmRadial", preProcess=c("center", "scale"), tuneGrid=svmGrid)
predictNoTime <- predict(fitNoTime, crossValNoTime)
confusionMatrix(crossValNoTime$classe, predictNoTime)

#Out of Sample Error
confusionMatrix(testNoTime$classe, predict(fitCols, testCols))
confusionMatrix(testNoTime$classe, predict(fitNoTime, testNoTime))

#Out of Sample Error
e2 <- confusionMatrix(testRaw$classe, predict(fitCols, testCols))
e3 <- confusionMatrix(testRaw$classe, predict(fitNoTime, testNoTime))

pml_write_files(predictFinalCols)

