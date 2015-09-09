fileUrlGDP <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileUrlEDU <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(fileUrlGDP, "./week3q3dataGDP.csv", method = "curl")
download.file(fileUrlEDU, "./week3q3dataEDU.csv", method = "curl")

dataGDPRaw <- read.csv("./week3q3dataGDP.csv", stringsAsFactors = FALSE, header = FALSE)
dataEDURaw <- read.csv("./week3q3dataEDU.csv", stringsAsFactors = FALSE, header = FALSE)

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

#q3
resultGDP <- dataGDPCountry[, 1:2]
resultEDU <- dataEDU[, 1:2]
resultDF <- arrange(join(resultGDP, resultEDU, type = "full"), desc(Ranking))
dim(dataGDPCountry)[1] + dim(resultEDU)[1] - dim(resultDF)[1]
resultDF[13,]

#q4
resultGDP <- dataGDPCountry[, 1:2]
resultEDU <- dataEDU[, 1:3]
resultDF <- arrange(join(resultGDP, resultEDU, type = "full"), desc(Ranking))
mean(resultDF[which(resultDF$"Income Group" == "High income: OECD"), ]$Ranking, na.rm = TRUE)
mean(resultDF[which(resultDF$"Income Group" == "High income: nonOECD"), ]$Ranking, na.rm = TRUE)

#q5
#need the resultDF from q4
resultDF <- arrange(resultDF[which(!is.na(resultDF$Ranking)),], Ranking)
library(Hmisc)
resultDF$GDPGroup <- cut2(resultDF$Ranking, g = 5)
table(resultDF$GDPGroup, resultDF$"Income Group")
