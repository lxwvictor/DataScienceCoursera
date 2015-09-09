#q1
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

library(caret)
set.seed(33833)
rfMod <- train(y ~ ., data = vowel.train, method = "rf")
gbmMod <- train(y ~ ., data = vowel.train, method = "gbm", verbose = FALSE)
rfPre <- predict(rfMod, vowel.test[,-1])
gbmPre <- predict(gbmMod, vowel.test[, -1])
confusionMatrix(rfPre, vowel.test$y)
confusionMatrix(gbmPre, vowel.test$y)

preDF <- data.frame(rfPre, gbmPre, y = vowel.test$y)
combMod <- train(y ~ ., method = "gbm", data = preDF, verbose = FALSE)
combPre <- predict(combMod, preDF)
confusionMatrix(combPre, vowel.test$y)

#q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
rfMod <- train(diagnosis ~ ., method = "rf", data = training)
set.seed(62433)
gbmMod <- train(diagnosis ~ ., method = "gbm", data = training, verbose = FALSE)
set.seed(62433)
ldaMod <- train(diagnosis ~ ., method = "lda", data = training)

rfPred <- predict(rfMod, testing[, -1])
gbmPred <- predict(gbmMod, testing[, -1])
ldaPred <- predict(ldaMod, testing[, -1])

combDF <- data.frame(rfPred, gbmPred, ldaPred, diagnosis = testing$diagnosis)
set.seed(62433)
combMod <- train(diagnosis ~., method = "rf", data = combDF)
combPred <- predict(combMod, combDF)

confusionMatrix(rfPred, testing$diagnosis)
confusionMatrix(gbmPred, testing$diagnosis)
confusionMatrix(ldaPred, testing$diagnosis)
confusionMatrix(combPred, testing$diagnosis)

#q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

lassoMod <- train(CompressiveStrength ~ ., method = "lasso", data = training)
plot(lassoMod)
plot(lassoMod$finalModel)

#q4
setwd("./Chapter8.Practical_Machine_Learning/")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", "gaData.csv", method = "curl")
if(require(lubridate) == FALSE) install.packages("lubridate")
library(lubridate)  # For year() function below
if(require(forecast) == FALSE) install.packages("forecast")
library(forecast)
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

batsFit <- bats(tstrain)
sum(forecast(batsFit, 235)$upper[,2] - testing$visitsTumblr < 0) / 235

#q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

if(require(e1071) == FALSE) install.packages("e1071")
library(e1071)
set.seed(325)
svmMod <- svm(training[, -9], training$CompressiveStrength)
svmPred <- predict(svmMod, testing[, -9])
sqrt(sum((svmPred - testing$CompressiveStrength)^2)/256)