## Prediction Motivation
if(require(caret) == FALSE) install.packages("caret")
library(caret)

## What is prediction
if(require(kernlab) == FALSE) install.packages("kernlab")
library(kernlab)
data(spam)
head(spam)

plot(density(spam$your[spam$type == "nonspam"]),
     col = "blue", main = "", xlab = "Frequence of 'your'")
lines(density(spam$your[spam$type == "spam"]), col = "red")
abline(v = 0.5, col = "black")

prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction, spam$type) / length(spam$type)

# In and out of sample errors
library(kernlab); data(spam); set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size = 10),]
spamLabel <- (smallSpam$type == "spam") * 1 + 1
plot(smallSpam$capitalAve, col = spamLabel)

rule1 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.7] <- "spam"
        prediction[x < 2.40] <- "nonspam"
        prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
        prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
        return(prediction)
}
table(rule1(smallSpam$capitalAve),smallSpam$type)

rule2 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.8] <- "spam"
        prediction[x <= 2.8] <- "nonspam"
        return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)

table(rule1(spam$capitalAve), spam$type)
table(rule2(spam$capitalAve), spam$type)
sum(rule1(spam$capitalAve) == spam$type)
sum(rule2(spam$capitalAve) == spam$type)

## The caret package
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

set.seed(32343)
if(require("e1071") == FALSE) install.packages("e1071")
modelFit <- train(type ~ ., data = training, method = "glm")
modelFit
modelFit$finalModel

predictions <- predict(modelFit, newdata = testing)
predictions
confusionMatrix(predictions, testing$type)

## Data slicing
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

# Return training set
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = TRUE,
                     returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Return test set
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = TRUE,
                     returnTrain = FALSE)
sapply(folds, length)
folds[[1]][1:10]

# Resampling
set.seed(32323)
folds <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# Time slices
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]

## Training options
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
modelFit <- train(type ~ ., data = training, method = "glm")

args(train.default)
args(trainControl)

set.seed(1235)
modelFit2 <- train(type ~ ., data = training, method = "glm")
modelFit2

## Plotting predictors
if(require(ISLR) == FALSE) install.packages("ISLR")
library(ISLR); library(ggplot2); library(caret)
data(Wage)
summary(Wage)

# Get training/test sets
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE) 
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training); dim(testing)

featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage, plot = "pairs")
qplot(age, wage, data = training)
qplot(age, wage, colour = jobclass, data = training)

qq <- qplot(age, wage, colour = education, data = training)
qq + geom_smooth(method = "lm", formula = y ~ x)

# cut2, making factors
if(require(Hmisc) == FALSE) install.packages("Hmisc")
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

# Boxplots with cut2
p1 <- qplot(cutWage, age, data = training, fill = cutWage,
            geom = c("boxplot"))
p1

# Boxplots with points overlayed
if(require(gridExtra) == FALSE) install.packages("gridExtra")
library(gridExtra)
p2 <- qplot(cutWage, age, data = training, fill = cutWage,
            geom = c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol = 2)

# Tables
t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1, 1)

# Density plots
qplot(wage, colour = education, data = training, geom = "density")

## Preprocessing
library(caret); library(kernlab); data(spam)
inTrain<- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
hist(training$capitalAve, main = "", xlab = "ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)

# Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

# Standardizing - test set, need to minus and divide by the training set, not test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

# Standardizing - preProcess function
preObj <- preProcess(training[, -58], method = c("center", "scale"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAves <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

# Standardizing - preProcess argument
set.seed(32343)
modelFit <- train(type ~ ., data = training, 
                  preProcess = c("center", "scale"), method = "glm")
modelFit

# Standardizing - Box-Cox transforms
preObj <- preProcess(training[,-58],method=c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# Standardizing - Imputting data
set.seed(13343)
# Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA

# Impute and standardize
if(require(RANN) == FALSE) install.packages("RANN")
library(RANN)
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])

## Covariate creation
library(kernlab);data(spam)
spam$capitalAveSq <- spam$capitalAve^2

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

# Removing zero covariates
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv

# Spline basis
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis

# Fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), col = "red", pch = 19, cex = 0.5)

# Splines on the test set
predict(bsBasis, age = testing$age)

## Preprocessing with principal components analysis
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

# Correlated predictors
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

# We could rotate the plot
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X, Y)

# Principal components in R - prcomp
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation

# PCA on SPAM data
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

# PCA with caret
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

# Preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)
testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

# Alternative (sets # of PCs) - one line
modelFit <- train(training$type ~ ., method = "glm", preProcess = "pca", data = training)
confusionMatrix(testing$type, predict(modelFit, testing))

## Predictions with Regression
# Example: Old faithful eruptions
library(caret);data(faithful); set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,
                               p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")

# Fit a linear model
lm1 <- lm(eruptions ~ waiting, data = trainFaith)
summary(lm1)

# Model fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

# Predict a new value
coef(lm1)[1] + coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

# Plot predictions - training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)

# Get training set/test set erros
# Calculate RMSE on training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# Calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

# Prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,col=c(1,2,2),lty = c(1,1,1), lwd=3)

# Same process with caret
modFit <- train(eruptions ~ waiting, data = trainFaith, method = "lm")
summary(modFit$finalModel)

## Predicting with regression, multiple covariates
# Example: predicting wages
library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

# Get training/test sets
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

# Plot age versus wage
qplot(age, wage, data = training)

# Plot age versus wage colour by jobclass
qplot(age, wage, colour = jobclass, data = training)

# plot age versus wage colour by education
qplot(age, wage, colour = education, data = training)

# Fit a linear model 
modFit <- train(wage ~ age + jobclass + education, method = "lm", data = training)
finMod <- modFit$finalModel
print(modFit)

# Diagnostics
plot(finMod,1,pch=19,cex=0.5,col="#00000010")

# Color by variables not used in the model
qplot(finMod$fitted,finMod$residuals,colour=race,data=training)

# Plot by index
plot(finMod$residuals,pch=19)

# Predicted versus truth in test set
pred <- predict(modFit, testing)
qplot(wage,pred,colour=year,data=testing)

# Use all covariates
modFitAll<- train(wage ~ .,data=training,method="lm")
pred <- predict(modFitAll, testing)
qplot(wage,pred,data=testing)

## Predicting with trees
# Example: Iris Data
data(iris); library(ggplot2); library(caret)
names(iris)
table(iris$Species)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Iris petal widths/sepal width
qplot(Petal.Width,Sepal.Width,colour=Species,data=training)

library(caret)
modFit <- train(Species ~ .,method="rpart",data=training)
print(modFit$finalModel)

# Plot Tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# Prettier plots
if(require(rattle) == FALSE) install.packages("rattle")
library(rattle)
fancyRpartPlot(modFit$finalModel)

# Predicting new values
predict(modFit, newdata = testing)

## Bagging
if(require(ElemStatLearn) == FALSE) install.packages("ElemStatLearn")
library(ElemStatLearn); data(ozone,package="ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

# Bagged loess
ll <- matrix(NA,nrow=10,ncol=155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1],replace=T)
  ozone0 <- ozone[ss,]; ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone,data=ozone0,span=0.2)
  ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for(i in 1:10){lines(1:155,ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

# More bagging in caret
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
if(require(party) == FALSE) install.packages("party")
library(party)
library(caret)
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone,predict(treebag,predictors),pch=19,col="blue")

# Parts of bagging
ctreeBag$fit
ctreeBag$pred
ctreeBag$aggregate

## Random forests
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

library(caret)
modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)
modFit

# Getting a single tree
getTree(modFit$finalModel,k=2)

# Class "centers"
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP); irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

# Predicting new values
pred <- predict(modFit,testing); testing$predRight <- pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")

## Boosting
# Wage example
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

# Fit the model
modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)

# Plot the results
qplot(predict(modFit,testing),wage,data=testing)

## Model based prediction
# Example: Iris Data
data(iris); library(ggplot2)
names(iris)
table(iris$Species)

# Create training and test sets
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Build predictions
modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)

# Comparison of results
equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)

## Regularized regression
# Prostate cancer 
library(ElemStatLearn); data(prostate)
str(prostate)

# Another issue for high-dimensional data
small = prostate[1:5,]
lm(lpsa ~ .,data =small)

## Combining predictors
# Example with Wage data
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)

# Building two different models
mod1 <- train(wage ~.,method="glm",data=training)
mod2 <- train(wage ~.,method="rf",
              data=training, 
              trControl = trainControl(method="cv"),number=3)

# Predict on the testing set
pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)

# Fit a model that combines predictors
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)

# Testing errors
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))

# Predict on validation data set
pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

# Evaluate on validation
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

## Forecasting
# Google data
if(require(quantmod) == FALSE) install.packages("quantmod")
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)
head(GOOG)

# Summarize monthly and store as time series
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")

# Decompose a time series into parts
plot(decompose(ts1),xlab="Years+1")

# Training and test sets
ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
ts1Train

# Simple moving average
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

# Exponential smoothing
ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")

# Get the accuracy
accuracy(fcast,ts1Test)

## Unsupervised prediction
# Iris example ignoring species labels
data(iris); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,
                               p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

# Cluster with k-means
kMeans1 <- kmeans(subset(training,select=-c(Species)),centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)

# Compare to real labels
table(kMeans1$cluster,training$Species)

# Build predictor
modFit <- train(clusters ~.,data=subset(training,select=-c(Species)),method="rpart")
table(predict(modFit,training),training$Species)

# Apply on test
testClusterPred <- predict(modFit,testing) 
table(testClusterPred ,testing$Species)