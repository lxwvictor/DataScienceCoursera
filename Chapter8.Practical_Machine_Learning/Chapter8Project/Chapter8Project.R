# Download and prepare data
library(randomForest)
if(require(beepr) == FALSE) install.packages("beepr")
library(beepr) # try it! beep()
if(require(pROC) == FALSE) install.packages("pROC")
library(pROC)

setwd("./Chapter8.Practical_Machine_Learning/Chapter8Project/")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              "pml_training.csv", method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              "pml_testing.csv", method = "curl")

# The "pml_training.csv" file contains all the training and validating data
pmlTraining <- read.csv("pml_training.csv")
# "pml_testing.csv" is for the purpose of this project 20 testing cases
testing <- read.csv("pml_testing.csv")

# Split data to training and validating with 7/3
inTrain <- createDataPartition(y = pmlTraining$classe, p = 0.7, list = FALSE)
training <- pmlTraining[inTrain, ]
validating <- pmlTraining[-inTrain, ]

# Take a glance at the data
dim(training)
str(testing)
summary(testing)

# Clean data by elimitating logical columns according to the testing data
t <- list()
for(i in 6:159) {
      if(class(testing[,i]) == "logical") t[length(t) + 1] <- i
}

newTraining <- training[, -c(1:5, unlist(t))]
newValidating <- validating[, -c(1:5, unlist(t))]
newTesting <- testing[, -c(1:5, unlist(t))]

# Converting data type as all columns are either integer or numeric
colNum <- dim(newTesting)[2] - 1
for(i in 1:colNum) {
        newTraining[, i] <- as.numeric(newTraining[, i])
        newValidating[, i] <- as.numeric(newValidating[, i])
        newTesting[, i] <- as.numeric(newTesting[, i])
}

# It takes about one minute to finish the training with mtry = 7 and ntree = 100
startTime <- proc.time()
rfModFit <- randomForest(classe ~ ., data = newTraining,
                         mtry = 7,
                         ntree = 40,
                         proximity = TRUE,
                         importance = TRUE)
beep()
proc.time() - startTime

plot(rfModFit$err.rate[, 1], type = "l", lwd = 3, col = "blue",
     main = "Random forest: OOB estimate of error rate",
     xlab = "Number of Trees", ylab = "OOB error rate")

tuneRF(subset(newTraining, select = -classe),
       newTraining$classe,
       ntreeTry = 100)
title("Random forest: tuning the mtry hyperparameter")

varImpPlot(rfModFit, main = "Random forest: Variable importance")

# Validating the model
rfPred <- predict(rfModFit, subset(newValidating, select = -classe))
table(rfPred, newValidating$classe)
sum(diag(table(rfPred, newValidating$classe)))/nrow(newValidating)

# Save the model
saveRDS(rfPred, file="myFile.rds")

# Load the model
rfPred = readRDS("myFile.rds")

# Predication on testing data
answers <- predict(rfModFit, subset(newTesting, select = - problem_id))
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}
pml_write_files(answers)
