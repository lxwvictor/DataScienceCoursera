complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the csv files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1 117
        ## 2 1041
        ## ...
        ## where 'id" is the monitor ID number and 'nobs" is the
        ## number of complete cases
        
        # Get a list of full file names
        files_full <- list.files(directory, full.names=TRUE)
        
        # Read all the files by given id
        target_files <- lapply(files_full[id], read.csv)
        
        # compute the complete cases for each target file
        nobs <- sapply(target_files, function(x) sum(complete.cases(x)))
        
        # return the required format
        data.frame('id' = id, 'nobs' = nobs)
}