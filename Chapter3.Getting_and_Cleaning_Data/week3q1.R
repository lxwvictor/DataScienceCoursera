fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./week3q1data.csv", method = "curl")

houseData <- read.csv("./week3q1data.csv", header = TRUE)
#targetHouse <- houseData[which(houseData$ACR == 3 & houseData$AGS == 6),]
agricultureLogical <- houseData$ACR == 3 & houseData$AGS == 6
which(agricultureLogical == TRUE)