library(RJSONIO)
if(require(rjson) == FALSE) install.packages("rjson")
library(rjson)

lines <- readLines("./yelp_academic_dataset_checkin.json")
checkin <- as.data.frame(t(sapply(lines, fromJSON)))

library(plyr)
con <- file("./yelp_academic_dataset_checkin.json", "r")
input <- readLines(con, -1L)
close(con)
checkin2 <- ldply(lapply(input, function(x) t(unlist(fromJSON(x)))))

checkin3 <- lapply(input, fromJSON)
checkin3 <- lapply(checkin3, cbind)
checkin3 <- as.data.frame(checkin3)
checkin3 <- as.data.frame(t(checkin3))

lines <- readLines("./yelp_academic_dataset_checkin.json")
oneLine <- lines[1]
oneLine
fromJSON(oneLine)
oneJSON <- t(fromJSON(oneLine))
oneJSON
oneJSON[1,1]

twoLines <- lines[1:2]
twoLines
twoJSON <- t(lapply(twoLines, fromJSON))
twoJSON <- data.frame(matrix(unlist(twoJSON)))
twoJSON

library(jsonlite)
search()
detach("package:jsonlite", unload = TRUE)

# Another way to read the json file
if(require(readr) == FALSE) install.packages("readr")
library(readr)
yelpBusinessDataFilePath <- "yelp_academic_dataset_user.json"
yelpBusinessData <- fromJSON(sprintf("[%s]", paste(read_lines(yelpBusinessDataFilePath), 
                                                   collapse = ",")), 
                             flatten=TRUE)
str(yelpBusinessData)


# Use stream_in to read the json files
if(require(jsonlite) == FALSE) install.packages("jsonlite")
if(require(doParallel) == FALSE) install.packages("doParallel")
library(jsonlite)
#library(doParallel)
#registerDoParallel(cores = 4)  #don't think this function will help

setwd("./Chapter10.Data_Science_Capstone/SourceData/yelp_dataset_challenge_academic_dataset/")
business <- stream_in(file("yelp_academic_dataset_business.json"))
checkin <- stream_in(file("yelp_academic_dataset_checkin.json"))
review <- stream_in(file("yelp_academic_dataset_review.json"))
tip <- stream_in(file("yelp_academic_dataset_tip.json"))
user <- stream_in(file("yelp_academic_dataset_user.json"))
saveRDS(business, "business.rds")
saveRDS(checkin, "checkin.rds")
saveRDS(review, "review.rds")
saveRDS(tip, "tip.rds")
saveRDS(user, "user.rds")

business <- readRDS("business.rds")
checkin <- readRDS("checkin.rds")
review <- readRDS("review.rds")
tip <- readRDS("tip.rds")
user <- readRDS("user.rds")

library(jsonlite)
businessf <- flatten(business)
checkinf <- flatten(checkin)
reviewf <- flatten(review)
tipf <- flatten(tip)
userf <- flatten(user)

saveRDS(businessf, "businessf.rds")
saveRDS(checkinf, "checkinf.rds")
saveRDS(reviewf, "reviewf.rds")
saveRDS(tipf, "tipf.rds")
saveRDS(userf, "userf.rds")

# Use below code to load in the processed data
setwd("./Chapter10.Data_Science_Capstone/SourceData/")
businessf <- readRDS("businessf.rds")
checkinf <- readRDS("checkinf.rds")
reviewf <- readRDS("reviewf.rds")
tipf <- readRDS("tipf.rds")
userf <- readRDS("userf.rds")

# quiz1 q7
table(businessf$`attributes.Wi-Fi`)[1]/sum(table(businessf$`attributes.Wi-Fi`))

# quiz1 q10
subset(userf, userf$compliments.funny > 10000)$name

# quiz1 q11
fans <- userf$fans
funny <- userf$compliments.funny
funny <- ifelse(is.na(funny), 0, funny)
tfans <- ifelse(fans == 0, 0, 1)
tfunny <- ifelse(funny == 0, 0, 1)
table(tfans, tfunny)
fisher.test(tfans, tfunny)


## Possible questions
# 1. Given the review of multiple restaurants, recommend the ones who may also like
# 2. Predict the reviews/number of customers on festivals, by districts/countries => not feasible as no info on review date
# 3. 


## Exploratory analysis
# Filter the US businesses
library(maps)
range(businessf$longitude)
range(businessf$latitude)
businessf$Country <- map.where(database = "world", businessf$longitude, businessf$latitude)
businessfUS <- businessf[which(businessf$Country == "USA"), ]
range(businessfUS$longitude)
range(businessfUS$latitude)

# Filter the restaurant businesses
businessfUSRst <- businessfUS[grepl("[Rr]estaurant", businessfUS$categories), ]
map("world", ylim=c(20,60), xlim=c(-130,-50), col="gray60")
points(businessfUSRst$longitude, businessfUSRst$latitude, pch = 19, col = 2)

# Perform k-means clustering, a west and east center will be generated
kcluster <- kmeans(cbind(businessfUSRst$longitude, businessfUSRst$latitude), 2)
points(kcluster$centers, pch = 10, col = "black", cex = 2)

businessfUSRstArea <- businessfUSRst
businessfUSRstArea$Area <- factor(kcluster$cluster, labels = c("East", "West"))

businessfUSRstEast <- subset(businessfUSRstArea, Area == "East")
businessfUSRstWest <- subset(businessfUSRstArea, Area == "West")
map("world", ylim=c(20,60), xlim=c(-130,-50), col="gray60")
points(businessfUSRstEast$longitude, businessfUSRstEast$latitude, pch = 19, col = "blue")
points(businessfUSRstWest$longitude, businessfUSRstWest$latitude, pch = 19, col = "red")
points(kcluster$centers, pch = 10, col = "black", cex = 2)
legend("bottomright", pch = c(19, 19, 10) , col = c("blue", "red", "black"), legend = c("East", "West", "Cluster Center"))

saveRDS(kcluster, "kcluster.rds")
saveRDS(businessfUSRstEast, "businessfUSRstEast.rds")
saveRDS(businessfUSRstWest, "businessfUSRstWest.rds")

# Verify all the business ids in the review file are valid
sum(is.na(match(reviewf$business_id, businessf$business_id)))

businessIndex <- match(reviewf$business_id, businessfUSRstArea$business_id)
reviewfArea <- reviewf
reviewfArea$Area <- businessfUSRstArea[businessIndex,]$Area

eastStars <- subset(reviewfArea, Area == "East")$stars
westStars <- subset(reviewfArea, Area == "West")$stars

par(mfrow = c(1,2))
boxplot(eastStars, col = "blue", xlab = "East")
boxplot(westStars, col = "red", xlab = "West")

t.test(eastStars, westStars)

saveRDS(eastStars, "eastStars.rds")
saveRDS(westStars, "westStars.rds")
