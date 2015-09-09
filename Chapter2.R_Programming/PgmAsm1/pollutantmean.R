pollutantmean <- function(directory, pollutant, id=1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ##  the locaiton of the csv files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        # Get a list of all the files in the given directory
        files_full <- list.files(directory, full.names = TRUE)
        
        # Read only those files by the given id, if id not given, then it's 1:322
        target_files <- lapply(files_full[id], read.csv)
        
        # Save only the pollutant data to 'pollutant_data'. Use anonymous function here
        pollutant_data <- sapply(target_files, function(x) x[, pollutant])
        mean(unlist(pollutant_data), na.rm = TRUE)
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vectors (ignoring NA valus)
}