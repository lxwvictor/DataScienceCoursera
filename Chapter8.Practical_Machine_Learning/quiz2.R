#q1
if(require(AppliedPreditiveModeling) == FALSE) install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

#q2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(log10(mixtures$Superplasticizer + 1))

#q3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

IL <- training[, grep("^IL", names(training))]
pre <- preProcess(IL, method = "pca", thresh = 0.8)
pre

#q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
ILTrain <- training[, grep("^IL", names(training))]
ILTrain$diagnosis <- training$diagnosis
ILTest <- testing[, grep("^IL", names(testing))]
ILTest$diagnosis <- testing$diagnosis

mdlFitNPCA <- train(diagnosis ~ ., data = ILTrain, method = "glm")
mdlFitNPCA
mdlFitNPCA$finalModel
confusionMatrix(ILTest$diagnosis, predict(mdlFitNPCA, ILTest))

preProc <- preProcess(ILTrain[, -13],method="pca",thresh=0.8)
ILTrainPC <- predict(preProc,ILTrain[, -13])
mdlFitPCA <- train(ILTrain$diagnosis ~ .,method="glm",data=ILTrainPC)
ILTestPC <- predict(preProc,ILTest[, -13])
confusionMatrix(ILTest$diagnosis,predict(mdlFitPCA,ILTestPC))
