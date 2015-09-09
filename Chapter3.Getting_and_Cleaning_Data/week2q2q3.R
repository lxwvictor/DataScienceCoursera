#install.packages("sqldf")

options(sqldf.driver = "SQLite")
library(sqldf)

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileUrl, destfile = "./getdata-data-ss06pid.csv", method = "curl")

acs <- read.csv("./getdata-data-ss06pid.csv")

sqldf("select pwgtp1 from acs where AGEP < 50")

sqldf("select distinct AGEP from acs")