complete <- function(directory, id = 1: 332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        cmpDf = data.frame(id = numeric(), nobs = numeric())    # data frame with 2 variables, 0 obs
        for (index in id) {
                # read the csv file by given index
                data <- read.csv(paste(directory, "/", sprintf("%03d", index), ".csv", sep=""))
                # bind the new data frame to existing data frame
                cmpDf <- rbind(cmpDf, data.frame(id = index, nobs = sum(complete.cases(data))))
        }
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        cmpDf
}