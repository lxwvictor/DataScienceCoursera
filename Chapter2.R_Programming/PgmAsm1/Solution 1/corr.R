corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        corVect <- numeric()
        cmpDf <- complete(directory)
        for (index in cmpDf$id) {
                if (cmpDf$nobs[index] > threshold) {
                        # read the csv file if the nobs is over the threshold
                        data <- read.csv(paste(directory, "/", sprintf("%03d", index), ".csv", sep=""))
                        # get the boolean vector of complete cases
                        isCmpCase <- complete.cases(data)
                        # get the data frame with complete cases only
                        cmpCase <- data[,][isCmpCase,]
                        # get the corralation of sulfate and nitrate, format it to 5 digits after
                        # decimal point, then convert it to numeric
                        #newVect <- as.numeric(sprintf("%.5f",cor(cmpCase$sulfate, cmpCase$nitrate)))
                        newVect <- cor(round(cmpCase$sulfate, digits=5), round(cmpCase$nitrate, digits= 5))
                        # combine the currect correlation vector to existing one
                        corVect <- c(corVect, newVect)
                }
        }
        
        #for (csvFile in list.files(directory)) {
        #        # read the csv file by given index
        #        #print(csvFile)
        #        data <- read.csv(paste(directory, "/", csvFile, sep=""))
        #        isCmpCase <- complete.cases(data)
        #        cmpCase <- data[,][isCmpCase,]
        #        numCmpCase <- sum(isCmpCase)
        #        if (numCmpCase > threshold) {
        #                newVect <- as.numeric(sprintf("%.5f",cor(cmpCase$sulfate, cmpCase$nitrate)))
        #                corVect <- c(corVect, newVect)
        #        }
        #}
        ## Return a numeric vector of correlations
        corVect
}