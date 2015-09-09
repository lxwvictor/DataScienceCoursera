## Download the provided zip file from online
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile = "./Electric_power_consumption.zip", method = "curl")
unzip("./Electric_power_consumption.zip")

## Get a sample of the data first to determin the class of each column and the column name.
sample <- read.table("./household_power_consumption.txt", header = TRUE, stringsAsFactors = FALSE,
        sep = ";", nrows = 10, na.strings = "?")
classes <- sapply(sample, class)

## Install "data.table" package if not installed.
if(require(data.table) == FALSE) install.packages("data.table")
library(data.table)

## Use fread function to read the data, it's much faster than "read.table" function.
## Moreover, the skip parameter is more powerful in fread function. 2 days of data
## from 1/2/2007.
dt <- fread("./household_power_consumption.txt", skip = "1/2/2007", nrows = 2*24*60)
## The data table doesn't have column names as the header also skipped, rename back.
names(dt) <- names(classes)

## Convert the data table to data frame as it's easier to manipulate the date and time.
hosPowCon <- as.data.frame.matrix(dt)
## Convert the string to Date type, "Y" in the formate string means 4 digit year.
hosPowCon$Date <- as.Date(hosPowCon$Date, format = "%d/%m/%Y")
## Convert the string to time type 
hosPowCon$Time <- strptime(paste(hosPowCon$Date, hosPowCon$Time), format = "%Y-%m-%d %H:%M:%S")