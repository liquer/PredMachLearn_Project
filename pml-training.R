# enable multi-core processing
library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
Sys.setlocale(locale="English")

## Define Error Rate: alpha = 0.05
## Reading in Data
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

## Cleaning Data
l <- vector(mode = "logical", length = 160)
for(i in 1:159){
  if(class(training[[i]]) == "factor"){
    l[i] <- TRUE
  }
}
training <- training[!l]
training <- subset(training, , select = -X)
training <- subset(training, , select = -raw_timestamp_part_1)
training <- subset(training, , select = -raw_timestamp_part_2)
testing <- testing[!l]
testing <- subset(testing, , select = -X)
testing <- subset(testing, , select = -raw_timestamp_part_1)
testing <- subset(testing, , select = -raw_timestamp_part_2)
for(i in 1:120){
  m <- mean(training[[i]], na.rm = TRUE)
  if(any(is.na(training[[i]]))){
    for(j in 1:19622){
      if(is.na(training[[i]][j])){
        training[[i]][j] <- m
      }
    }
  }
  if(any(is.na(testing[[i]]))){
    for(j in 1:20){
      if(is.na(testing[[i]][j])){
        testing[[i]][j] <- m
      }
    }
  }
}

## Split Data
library(caret)
set.seed(32323)
inTrain <- createDataPartition(y = training$classe, p = 0.6, list = FALSE)
validation <- training[-inTrain, ]
training <- training[inTrain, ]

## On the Training Set Pick Prediction Functions
fit1 <- train(classe ~ ., data = training, method = "rf", 
              trControl = trainControl(method = "cv", number = 10))
fit2 <- train(classe ~ ., data = training, method = "gbm", verbose = FALSE, 
              trControl = trainControl(method = "cv", number = 10))

## Validation
predv1 <- predict(fit1, newdata = validation)
confuMatrix1 <- confusionMatrix(predv, validation$classe)
predv2 <- predict(fit2, newdata = validation)
confuMatrix2 <- confusionMatrix(predv2, validation$classe)

## Combining Predictors
predDF <- data.frame(predv1, predv2, classe = validation$classe)
combfit <- train(classe ~ ., data = predDF)
combPred <- predict(combfit, predDF)
combconfuMatrix <- confusionMatrix(combPred, predDF$classe)

## Apply 1x to Testing Set
answers <- predict(fit1, newdata = testing)
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
