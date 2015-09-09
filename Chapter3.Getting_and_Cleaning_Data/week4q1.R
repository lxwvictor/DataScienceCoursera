fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "week4q1data.csv", method = "curl")
dataHousing <- read.csv("./week4q1data.csv", header = TRUE)

strsplit(names(dataHousing), "wgtp")[123]
