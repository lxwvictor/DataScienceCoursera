# Chapter3Project
## Introduction
This is the project of "Getting and Cleaning Data" of "Data Science" specialization from Coursera. All the raw file(s), source code and result file(s) are included in this repo.

## Repo structure
According to the project requirement, below files are mandatory. Clone this repo or just the _run\_analysis.R_ and then run command `Rscript run_analysis.R`, the R script will download, read, manipulate data and create the result data set named _summarisedData.txt_ in the working directory.

* _run\_analysis.R_: The source code of R script and "run_analysis.R" is the required name.
* _README.md_: This markdown file.
* _README.html_: The resulting html file from the _README.md_ file.
* _CodeBook.md_: The code book describes the variables and summaries.
* _CodeBook.html_: The resulting html file from the _CodeBook.md_ file.
* _summarisedData.txt_: The output file of the tidy data set with the average of each variable for each activity and each subject. The code to read this file can be `summarisedData <- read.table("./summarisedData.txt", header = TRUE)`.

While below files are not mandatory, as the source code in "run_analysis.R" will download and unzip the raw files.

* _dataset.zip_: The downloaded zip file from the given url.
* _UCI HAR Dataset_: The result folde after unzip the _dataset.zip_ file. It's essential to understand what the file(s) and folder(s) in this folder are about.
    + _activity_labels.txt_: The 6 different activities.
    + _features.txt_: The 561 different features.
    + _features\_info.txt_: A brief introduction about the how the features were calculated and naming convention.
    + _README.md_: The readme file provides a brief background of how the raw data was collected and the introduction to the files.
    + _test_: The test group.
        + _subject\_test.txt_: The subject indicators for each row of data test group.
        + _X\_test.txt_: The data set in test group.
        + _y\_test.txt_: The activity indicators for each row of data in test group.
        + _Inertial Signals_: The very raw data, not actually in use for this project.
    + _train_: The train group.
        + _subject\_train.txt_: The subject indicators for each row of data in train group.
        + _X\_train.txt_: The data set in train group.
        + _y\_train.txt_: The activity indicators for each row of data in train group.
        + _Inertial Signals_: The very raw data, not actually in use for this project.

The _run\_analysis.R_ will work either with or without the raw data as it will download the raw data.

## run_analysis.R
This is the R script that will handle all the tasks stated in the project requirement. There are comments in the script file explaining the purpose of the source code and how it is achieved. The script can be devided into 3 modules. Prepare data, read data and minpulate data. Detailed information about the variables, summarised data in the data set are included in the _CodeBook.md_

* Prepare Data: Download the data by the given url and save to the current working directory. The downloaded file named _dataset.zip_. Then use the `unzip` function to unzip to the working directory, a new folder named _UCI HAR Dataset_ will be created.
* Read Data: Read the _x_, _y_ and _subject_ data from the _test_ and _train_ folder. As well as the _features_ and _activity\_labels_ data.
* Manipulate Data:
    1. Merges the training and test data: All the data needed has be read in previous 2 modules. By checking the dimention, head and tail of each variable we can conclude the data actually are quite tidy. A simple `cbind` and `rbind` will be able to merge the test data, train data, subject indicators and activity indicators into a single data set as required. To make the steps later easier, the column names of subject and activity indicators have updated to more discriptive names.
    2. Extracts only the measurements on the mean and standard deviation: This step is quite subjective as different people have different views on which measurement is considered as mean or standard deviation. I followed the _features\_info.txt_ file and decided only those measurements contain string of either "mean()", "std()" or "meanFreq()" are eligible. This resulted in 79 measurements. Use the `grep` function will return a vector indicating the column index of eligible measurements. Then subsetting accordingly. Finally, to make the following steps easier and the resulting data set more reasonable and complete, I used `cbind` to bind the _subject_ and _activity_ columns.
    3. Uses discriptive activity names to name the activities: From the _activity\_labels.txt_ file we can tell there are 6 different activities corresponding to number 1 to 6. The _activity_ column from previous data set is the activity indicator column, value from 1 to 6. Need to convert to the more descriptive names. Here I converted the column to factor first and then assign the activity names to the levels of the factor. Thus, the activity indicators will be converted to activity names/labels in one go.
    4. Appropriately labels the data set with descriptive variable names: In the data set generate from previous step, the column names are still the default column names of _V1_, _V2_..., expect the last 2 columns were named manually in previous steps. The nice thing about the column names is the digits are still telling the index of the matching feature name in _features.txt_. For example, "V45" means the 45th feature, although it's the 11th column in the data set. I used `gsub` and `as.numeric` to convert the column names into feature index and then assign the feature names to the column names. Special characters "(", ")" and "-" were removed, first letter of "mean" and "std" were capitalized to make the column names more readable and supported by R when read and write to files.
    5. From step 4, create a tidy data set with average of each variable for each activity and each subject: A very tidy and complete data set has already been created in previous step. Each column is a different measured variable, each row is a different observation. The _subject_ and _activity_ column indicates the observation belongs to which subject and about which kind of activity. So here I utilize `group_by` and `summarise_each` function from `dplyr` package to create the final tidy data set. The result data set is a 180 * 81 data frame, because there are 30 subjects, 6 actvities, 79 chosen measurements and 2 columns indication subject index and activity names. Finally, the data set is saved to "summarisedData.txt" using `write.table()` function.

## Thanks to all the contributors in the forum discussion. It really helped me a lot.
[Forum Discussion](https://class.coursera.org/getdata-012/forum/list?forum_id=10009)