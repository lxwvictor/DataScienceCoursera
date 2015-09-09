# Define the url, download the data by the url provided and unzip it to the working directory
dataUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataUrl, destfile = "./dataset.zip", method = "curl")
unzip("./dataset.zip")

# Define the relative location of "UCI HAR Dataset" folder, then read the files
# X_train.txt:         the data of all the features from training group
# y_train.txt:         the 6 different activities for the data in training group
# subject_train.txt:   the 30 subjects in training group
# X_test.txt:          the data of all features from testing group
# y_test.txt:          the 6 different activities for the data in testing group
# activity_labels.txt: the 6 different activities
# features.txt:        there are 561 features which are basically come from the sensor
# subject_test.txt:    the 30 subjects in testing group

# The variable names all start with small case letter, and follow the file names
dataFolder <- "./UCI HAR Dataset"
x_train <- read.table(paste(dataFolder, "train/X_train.txt", sep = "/"), stringsAsFactors = FALSE)
y_train <- read.table(paste(dataFolder, "train/y_train.txt", sep = "/"), stringsAsFactors = FALSE)
subject_train <- read.table(paste(dataFolder, "train/subject_train.txt", sep = "/"), stringsAsFactors = FALSE)

x_test <- read.table(paste(dataFolder, "test/X_test.txt", sep = "/"), stringsAsFactors = FALSE)
y_test <- read.table(paste(dataFolder, "test/y_test.txt", sep = "/"), stringsAsFactors = FALSE)
subject_test <- read.table(paste(dataFolder, "test/subject_test.txt", sep = "/"), stringsAsFactors = FALSE)

activityLabels <- read.table(paste(dataFolder, "activity_labels.txt", sep = "/"), stringsAsFactors = FALSE)
features <- read.table(paste(dataFolder, "features.txt", sep = "/"), stringsAsFactors = FALSE)

## Step 1
# rbind the x_test and x_train as they are the same type of data, except in testing and training groups
# Applies to same to y_test and y_train, subject_test and subject_train
x <- rbind(x_test, x_train)
y <- rbind(y_test, y_train)
subject <- rbind(subject_test, subject_train)
        
# Name the column names of y and subject appropriately before binding to x
# Merge x, y and subjects by cind. Meaning, created 2 new columns "activity" and "subject
names(y) <- "activity"
names(subject) <- "subject"
oneDataset <- cbind(x, y, subject)


## Step 2
# The definition of measurements of mean and standard deviation is quite objective. I followed
# the "features_info.txt" file to select the measurements if the columen names contains either
# of "mean()", "std()" or "meanFreq()"
containsMeanStd <- grep("mean\\(\\)|std\\(\\)|meanFreq\\(\\)", features[, 2], ignore.case = TRUE)

# Subset the "oneDataset" by the "containsMeanStd"
measureMeanStd <- oneDataset[, containsMeanStd]

# cbind the activity and subject column
measureMeanStd <- cbind(measureMeanStd, activity = oneDataset$activity, subject = oneDataset$subject)


## Step 3
# Convert the activity indicators to activity names. This can be achieved by converting the "activity"
# column into factors first, then assign the activity names/labels to the levels of the factor column
measureMeanStd$activity <- as.factor(measureMeanStd$activity)
levels(measureMeanStd$activity) <- activityLabels[, 2]


## Step 4
# Assign the column names based on the feature names. The column names of "measureMeanStd" are still
# in the form like "V1", "V2" and etcs. The nice thing about the column name is the digits are still
# telling the index of the matching feature name in "features" data frame. For example, "V45" means the
# 45th feature, although it's the 11th column in the "measureMeanStd" data frame.

# Replace "V" into "", coerce into numeric, replace the special characters "(", ")", "-"
colNameIndex <- as.numeric(gsub("V", "", names(measureMeanStd)))
colNameIndex <- colNameIndex[!is.na(colNameIndex)]
names(measureMeanStd)[1:length(colNameIndex)] <- features[colNameIndex, 2]
names(measureMeanStd) <- gsub("\\(", "", names(measureMeanStd))
names(measureMeanStd) <- gsub("\\)", "", names(measureMeanStd))
names(measureMeanStd) <- gsub("\\-", "", names(measureMeanStd))

# Capitalize the first letter of "mean" and "std" to make the column names more readable and descriptive
# "fBodyGyroMeanFreqX" is more descriptive than "fBodyGyromeanFreqX"
names(measureMeanStd) <- gsub("mean", "Mean", names(measureMeanStd))
names(measureMeanStd) <- gsub("std", "Std", names(measureMeanStd))


## Step 5
# Use the "group_by" and "summarize_each" function from "dplyr" package to calculate the mean of each
# measure for each subject and activity. The result output is stored in "summarisedData.txt".
# summarizedData <- read.table("./summarisedData.txt", header = TRUE) will read the data
library(dplyr)
groupedData <- group_by(measureMeanStd, subject, activity)
summarisedData <- summarise_each(groupedData, funs(mean))
write.table(summarisedData, file = "./summarisedData.txt", row.name = FALSE)
