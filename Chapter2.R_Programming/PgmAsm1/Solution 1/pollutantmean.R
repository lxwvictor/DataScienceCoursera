pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate"
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        # total is the sum of pollutant
        # count is the number of valid data, excluding the NA
        #total <- 0
        #count <- 0
        polData <- numeric()    # a numeric vector with 0 length
        for (index in id) {
                # read the csv file based on the directory variable and the index value.
                # index will be coerced to string with length of 3 with leading 0s.
                # sep must be set to ""
                data <- read.csv(paste(directory, "/", sprintf("%03d", index), ".csv", sep=""))
                polData <- c(polData, as.numeric(data[[pollutant]]))    # combine 2 vectors
                #polData <- as.numeric(data[[pollutant]])
                #total <- total + sum(polData, na.rm = TRUE)
                #count <- count + sum(complete.cases(polData))
        }
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id" vector (ignoring NA values)
        #pollutantmean = total/count
        
        # keep 3 digits after decimal point
        #sprintf("%.3f", pollutantmean)
        as.numeric(sprintf("%.3f", mean(polData, na.rm = TRUE)))
}