corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        # Get the data of how many complete cases in each csv file
        # from previous 'complete' function
        complete_files <- complete(directory)
        
        # Get the list of files in the given directory
        files_full <- list.files(directory, full.names = TRUE)
        
        # create an empty numeric vector
        corrVect <- numeric()
        for (i in complete_files$id) {  # if over threshold
                if (complete_files$nobs[i] > threshold) {
                        # read the data of the csv file
                        data <- read.csv(files_full[i])
                        # get the complete cases of this csv file
                        isComplete <- complete.cases(data)
                        # extract only the complete cases
                        compData <- data[isComplete, ]
                        # calculate the correlation
                        newCorr <- cor(compData$sulfate, compData$nitrate)
                        # combine the new correlation to existing correlation
                        corrVect = c(corrVect, newCorr)
                }
        }
        corrVect
        ## Return a numeric vector of correlations
}