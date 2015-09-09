fileUrlGDP <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileUrlEDU <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(fileUrlGDP, "./week4q2toq4dataGDP.csv", method = "curl")
download.file(fileUrlEDU, "./week4q2toq4dataEDU.csv", method = "curl")

dataGDPRaw <- read.csv("./week4q2toq4dataGDP.csv", stringsAsFactors = FALSE, header = FALSE)
dataEDURaw <- read.csv("./week4q2toq4dataEDU.csv", stringsAsFactors = FALSE, header = FALSE)

dataGDP <- dataGDPRaw[6:236, c(1:2, 4:5)]
names(dataGDP) <- c("CountryCode", "Ranking", "Country", "GDP")
dataGDP$Ranking <- as.numeric(dataGDP$Ranking)
dataGDP$GDP <- gsub(" ", "", dataGDP$GDP)
dataGDP$GDP <- gsub("\\.\\.", "", dataGDP$GDP)
dataGDP$GDP <- gsub("\\,", "", dataGDP$GDP)
dataGDP$GDP <- as.numeric(dataGDP$GDP)
dataGDPCountry <- dataGDP[which(dataGDP$CountryCode != "" & !is.na(dataGDP$Ranking)),]

dataEDU <- dataEDURaw
names(dataEDU) <- dataEDU[1,]
dataEDU <- dataEDU[-1,]

#q2
mean(dataGDP$GDP[1:195], na.rm = TRUE)

#q3
grep("^United", dataGDP$Country)

#q4
mergedData <- merge(dataGDP, dataEDU, by = "CountryCode", all = TRUE)
sum(grepl("Fiscal year end: June 30", mergedData$"Special Notes"))