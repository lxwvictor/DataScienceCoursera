read_data <- function() {
        file_name = "repdata-data-StormData.csv.bz2"
        Url = "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        if(!file.exists(file_name)) {
                download.file(Url, destfile = file_name)
        }
        StormData <- read.csv(bzfile(file_name), header = T)
        StormData
}
if(exists("StormData") == FALSE) {StormData <- read_data()}

grepl("[Hh]", "adsfh")
