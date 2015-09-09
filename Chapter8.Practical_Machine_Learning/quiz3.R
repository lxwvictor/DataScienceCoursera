library(AppliedPredictiveModeling); library(caret); library(ElemStatLearn); library(pgmm)
library(rpart)

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

#q1
testing <- subset(segmentationOriginal, Case == "Test")
training <- subset(segmentationOriginal, Case == "Train")
set.seed(125)
modFit <- train(Class ~ ., method = "rpart", data = training[, c(-1, -2)])
print(modFit$finalModel)
plot(modFit$finalModel, uniform = TRUE)
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.5)

all.columns <- mapply(function(x) NA, names(training), USE.NAMES = T)

make.obs <- function(repl){
        # replace NAs in all.columns with data from repl and return as data.frame
        data.frame(t(replace(all.columns, names(repl), repl)))
}

newdata.a <- make.obs(c(TotalIntench2 = 23000, FiberWidthCh1 = 10, PerimStatusCh1=2 ))
newdata.d <- make.obs(c(FiberWidthCh1 = 8, VarIntenCh4 = 100, PerimStatusCh1=2 ))
predict(modFit, newdata = newdata.d)

#q3
library(pgmm)
data(olive)
olive = olive[,-1]
modFit3 <- train(Area ~ ., method = "rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit3, newdata)

#q4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
regMod <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, family = "binomial",
              trainSA)
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
trainPre <- predict(regMod, trainSA)
testPre <- predict(regMod, testSA)
missClass(trainSA$chd, trainPre)
missClass(testSA$chd, testPre)

#q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- factor(vowel.train$y)
vowel.test$y <- factor(vowel.test$y)
set.seed(33833)
modFit5 <- train(y ~ ., method = "rf", data = vowel.train)
varImp(modFit5)

pwpn
the bias is larger
2.783
0.31, 0.27
2156