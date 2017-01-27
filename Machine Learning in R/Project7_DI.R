library("randomForest")
library("caret")
library("kernlab")

weight <- read.csv("weightlift.csv")
total_vars <- ncol(weight)


# Cross-validation
trainOpts <- trainControl()
trainOpts$method = "cv"

set.seed(3792)
inTraining <- createDataPartition(y=weight$classe, p=0.05, list=FALSE)
trainingData <- weight[inTraining,]
validationData <- weight[-inTraining,]

cvmodel_weight <- train(classe~., data=trainingData, method="rf",trControl=trainOpts)

# Selecting my variables
best <- varImp(cvmodel_weight)
best_sort <- order(best$importance$Overall, decreasing = TRUE)
keep <- row.names(best$importance)[best_sort[1:30]]

validationData <- validationData[,c(keep,"classe")]
  
inTraining <- createDataPartition(y=validationData$classe, p=0.2, list=FALSE)
trainingAgain <- validationData[inTraining,]
testing <- validationData[-inTraining,]


rfmodel2 <- train(classe~., data = trainingAgain, method = "rf", trControl = trainOpts)

rfmodel2$finalModel
testPredict <- predict(rfmodel2, testing)
confusionMatrix(testPredict, testing$classe)$table
confusionMatrix(testPredict, testing$classe)$overall[1]

