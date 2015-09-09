# Chapter3Project Code Book

## Introduction
This is the code book file in the the project of "Getting and Cleaning Data" of "Data Science" specialization from Coursera.

## Backgroud
Refer to "http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones" for more details of the background of the data set.

The original raw data set is available from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip".

## The Summarised Data
Download the _run\_anaylysis.R_ from the repo to local drive, in the command prompt, run below command.
```
Rscript run_analysis.R
```
The script will download and minipulate the data, eventually created a _summarisedData.txt_ in the working directory as the output summarised data.
Or run below command in R to load in the data set already created given that the repo has been cloned to local drive and working directory has been set to it.
```
summarisedData <- read.table("./summarisedData.txt", header = TRUE)
```
The details of how the script runs can be found the _README.md_ file in the same repo.

The loaded data set is a data frame with dimension of 180 * 81, it was created by calculating the average of measurements which are on the mean and standard deviation of certain sensor data.

In the data frame, there are 30 different subjects, 6 different activities, 79 different types of measurements value range in [-1, 1]. The variable/column name for each measurement is quite self-explained.

* t: prefix, time domain signal
* f: prefix, frequence domain signal
* Body: Body
* Acc: Acceleration signal measured by the accelerometer
* Gyro: Angular velocity measured by gyroscope
* Jerk: Jerk signal was derived from the accelaration and angular velocity. It's the rate of change of acceleration
* Mean: Mean
* MeanFreq: Weighted average of the frequency components
* Std: Standard deviation
* Mag: The magnitude of the signal
* Gravity: Gravity
* X: signal on X-axis
* Y: signal on Y-axis
* Z: signal on Z-axis 

The detailed information about each variable/column name are explained as follows.

| Variable Name  | Description     |
| :------------- |:--------------- |
| subject | The indicator of subjects, range from 1 to 30 |
| activity | The types of activity, range in "WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING" |
| tBodyAccMeanX | Time signal, the mean of body acceleration on X-axis |
| tBodyAccMeanY | Time signal, the mean of body acceleration on Y-axis |
| tBodyAccMeanZ | Time signal, the mean of body acceleration on Z-axis |
| tBodyAccStdX | Time signal, the standard deviation of body acceleration on X-axis |
| tBodyAccStdY | Time signal, the standard deviation of body acceleration on Y-axis |
| tBodyAccStdZ | Time signal, the standard deviation of body acceleration on Z-axis |
| tGravityAccMeanX | Time signal, the mean of gravity acceleration on X-axis |
| tGravityAccMeanY | Time signal, the mean of gravity acceleration on Y-axis |
| tGravityAccMeanZ | Time signal, the mean of gravity acceleration on Z-axis |
| tGravityAccStdX | Time signal, the standard deviation of gravity acceleration on X-axis |
| tGravityAccStdY | Time signal, the standard deviation of gravity acceleration on Y-axis |
| tGravityAccStdZ | Time signal, the standard deviation of gravity acceleration on Z-axis |
| tBodyAccJerkMeanX | Time signal, the mean of body acceleration jerk on X-axis |
| tBodyAccJerkMeanY | Time signal, the mean of body acceleration jerk on Y-axis |
| tBodyAccJerkMeanZ | Time signal, the mean of body acceleration jerk on Z-axis |
| tBodyAccJerkStdX | Time signal, the standard deviation of body acceleration jerk on X-axis |
| tBodyAccJerkStdY | Time signal, the standard deviation of body acceleration jerk on Y-axis |
| tBodyAccJerkStdZ | Time signal, the standard deviation of body acceleration jerk on Z-axis |
| tBodyGyroMeanX | Time signal, the mean of body angular velocity on X-axis |
| tBodyGyroMeanY | Time signal, the mean of body angular velocity on Y-axis |
| tBodyGyroMeanZ | Time signal, the mean of body angular velocity on Z-axis |
| tBodyGyroStdX | Time signal, the standard deviation of body angular velocity on X-axis |
| tBodyGyroStdY | Time signal, the standard deviation of body angular velocity on Y-axis |
| tBodyGyroStdZ | Time signal, the standard deviation of body angular velocity on Z-axis |
| tBodyGyroJerkMeanX | Time signal, the mean of body angular velocity jerk on X-axis |
| tBodyGyroJerkMeanY | Time signal, the mean of body angular velocity jerk on Y-axis |
| tBodyGyroJerkMeanZ | Time signal, the mean of body angular velocity jerk on Z-axis |
| tBodyGyroJerkStdX | Time signal, the standard deviation of body angular velocity jerk on X-axis |
| tBodyGyroJerkStdY | Time signal, the standard deviation of body angular velocity jerk on Y-axis |
| tBodyGyroJerkStdZ | Time signal, the standard deviation of body angular velocity jerk on Z-axis |
| tBodyAccMagMean | Time signal, the mean of magnitude of body acceleration |
| tBodyAccMagStd | Time signal, the standard deviation of magnitude of body acceleration |
| tGravityAccMagMean | Time signal, the mean of magnitude of gravity acceleration |
| tGravityAccMagStd | Time signal, the standard deviation of magnitude of gravity acceleration |
| tBodyAccJerkMagMean | Time signal, the mean of magnitude of body acceleration jerk |
| tBodyAccJerkMagStd | Time signal, the standard deviation of magnitude of body acceleration jerk |
| tBodyGyroMagMean | Time signal, the mean of magnitude of body angular velocity |
| tBodyGyroMagStd | Time signal, the standard deviation of magnitude of body angular velocity |
| tBodyGyroJerkMagMean | Time signal, the mean of magnitude of body angular velocity jerk |
| tBodyGyroJerkMagStd | Time signal, the standard deviation of magnitude of body angular velocity jerk |
| fBodyAccMeanX | Frequency signal, the mean of body acceleration on X-axis |
| fBodyAccMeanY | Frequency signal, the mean of body acceleration on Y-axis |
| fBodyAccMeanZ | Frequency signal, the mean of body acceleration on Z-axis |
| fBodyAccStdX | Frequency signal, the standard deviation of body acceleration on X-axis |
| fBodyAccStdY | Frequency signal, the standard deviation of body acceleration on Y-axis |
| fBodyAccStdZ | Frequency signal, the standard deviation of body acceleration on Z-axis |
| fBodyAccMeanFreqX | Frequency signal, the weighted mean of body acceleration on X-axis |
| fBodyAccMeanFreqY | Frequency signal, the weighted mean of body acceleration on Y-axis |
| fBodyAccMeanFreqZ | Frequency signal, the weighted mean of body acceleration on Z-axis |
| fBodyAccJerkMeanX | Frequency signal, the mean of body acceleration jerk on X-axis |
| fBodyAccJerkMeanY | Frequency signal, the mean of body acceleration jerk on Y-axis |
| fBodyAccJerkMeanZ | Frequency signal, the mean of body acceleration jerk on Z-axis |
| fBodyAccJerkStdX | Frequency signal, the standard deviation of body acceleration jerk on X-axis |
| fBodyAccJerkStdY | Frequency signal, the standard deviation of body acceleration jerk on Y-axis |
| fBodyAccJerkStdZ | Frequency signal, the standard deviation of body acceleration jerk on Z-axis |
| fBodyAccJerkMeanFreqX | Frequency signal, the weighted mean of body acceleration jerk on X-axis |
| fBodyAccJerkMeanFreqY | Frequency signal, the weighted mean of body acceleration jerk on Y-axis |
| fBodyAccJerkMeanFreqZ | Frequency signal, the weighted mean of body acceleration jerk on Z-axis |
| fBodyGyroMeanX | Frequency signal, the mean of body angular velocity on X-axis |
| fBodyGyroMeanY | Frequency signal, the mean of body angular velocity on Y-axis |
| fBodyGyroMeanZ | Frequency signal, the mean of body angular velocity on Z-axis |
| fBodyGyroStdX | Frequency signal, the standard deviation of body angular velocity on X-axis |
| fBodyGyroStdY | Frequency signal, the standard deviation of body angular velocity on Y-axis |
| fBodyGyroStdZ | Frequency signal, the standard deviation of body angular velocity on Z-axis |
| fBodyGyroMeanFreqX | Frequency signal, the weighted mean of body angular velocity on X-axis |
| fBodyGyroMeanFreqY | Frequency signal, the weighted mean of body angular velocity on Y-axis |
| fBodyGyroMeanFreqZ | Frequency signal, the weighted mean of body angular velocity on Z-axis |
| fBodyAccMagMean | Frequency signal, the mean of body acceleration magnitude |
| fBodyAccMagStd | Frequency signal, the standard deviation of body acceleration magnitude |
| fBodyAccMagMeanFreq | Frequency signal, the weighted mean of body acceleration magnitude |
| fBodyBodyAccJerkMagMean | Frequency signal, the mean of the magnitude of body acceleration jerk |
| fBodyBodyAccJerkMagStd | Frequency signal, the standard deviation of the magnitude of body acceleration jerk |
| fBodyBodyAccJerkMagMeanFreq | Frequency signal, the weighted mean of the magnitude of body acceleration jerk |
| fBodyBodyGyroMagMean | Frequency signal, the mean of magnitude of body angular velocity |
| fBodyBodyGyroMagStd | Frequency signal, the standard deviation of magnitude of body angular velocity |
| fBodyBodyGyroMagMeanFreq | Frequency signal, the weighted mean of magnitude of body angular velocity |
| fBodyBodyGyroJerkMagMean | Frequency signal, the mean of magnitude of body angular velocity jerk |
| fBodyBodyGyroJerkMagStd | Frequency signal, the standard deviation of magnitude of body angular velocity jerk |
| fBodyBodyGyroJerkMagMeanFreq | Frequency signal, the weighted mean of magnitude of body angular velocity jerk |

## Thanks to all the contributors in the forum discussion.
[Forum Discussion](https://class.coursera.org/getdata-012/forum/list?forum_id=10009)