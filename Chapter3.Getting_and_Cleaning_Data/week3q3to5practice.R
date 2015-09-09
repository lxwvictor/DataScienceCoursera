library(downloader)
download("https://raw.githubusercontent.com/thoughtfulbloke/faoexample/master/appleorange.csv",
         destfile = "./appleorange.csv")
download("https://raw.githubusercontent.com/thoughtfulbloke/faoexample/master/stability.csv",
         destfile = "./stability.csv")

ao <- read.csv("appleorange.csv")
str(ao)

aoraw <- read.csv("appleorange.csv", stringsAsFactors = FALSE, header = FALSE)
head(aoraw)
tail(aoraw)

aodata <- aoraw[3:700,]
names(aodata) <- c("country", "countrynumber", "products", "productnumber", "tonnes", "year")
aodata$countrynumber <- as.integer(aodata$countrynumber)

fslines <- which(aodata$country == "Food supply quantity (tonnes) (tonnes)")
aodata <- aodata [(-1 * fslines),]
# or below line also the same effect
# aodata <- aodata[which(aodata$country!= "Food supply quantity (tonnes) (tonnes)"),]

aodata$tonnes <- gsub("\xca", "", aodata$tonnes)
aodata$tonnes <- gsub(", tonnes \\(\\)", "", aodata$tonnes)
aodata$tonnes <- as.numeric(aodata$tonnes)
aodata$year <- 2009

cleanao1 <- aodata[aodata$productnumber == 2617, ]
cleanao1$oranges <- aodata$tonnes[aodata$productnumber == 2611]
cleanao1 <- cleanao1[, c(1, 2, 5, 7)]
names(cleanao1)[names(cleanao1) == "tonnes"] <- "apples"

apples <- aodata[aodata$productnumber == 2617, c(1, 2, 5)]
names(apples)[3] <- "apples"
oranges <- aodata[aodata$productnumber == 2611, c(2, 5)]
names(oranges)[2] <- "oranges"
cleanao2 <- merge(apples, oranges, by = "countrynumber", all = TRUE)

library(reshape2)
cleanao3 <- dcast(aodata[, c(1:3, 5)], formula = country + countrynumber ~ products, value.var = "tonnes")
names(cleanao3)[3:4] <- c("apples", "oranges")

cleanao2[!(complete.cases(cleanao2)),]
cleanao3[!(complete.cases(cleanao3)),]

table(aodata$country)[table(aodata$country) == 1]
