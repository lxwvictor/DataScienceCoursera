setwd("./Chapter3.Getting_and_Cleaning_Data/")
# Downloading Files
if(!file.exists("data")) {
        dir.create("data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl")
list.files("./data")
dateDownloaded <- date()

# Reading Local Files
cameraData <- read.table("./data/cameras.csv", sep = ",", header = TRUE)
head(cameraData)

cemeraData <- read.csv("./data/cameras.csv")
head(cameraData)
##some other parameters: quote, na.strings, nrows, skip

# Reading Excel Files
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.xlsx", method = "curl")
dateDownloaded <- date()

library(xlsx)
##Below line won't work as the file format is actually csv, can't use read xlsx
cameraData <- read.xlsx("./data/cameras.xlsx", sheetIndex = 1, header = TRUE)
head(cameraData)

colIndex <- 2:3
rowIndex <- 1:4
cameraDataSubset <- read.xlsx("./data/cameras.xlsx", sheetIndex = 1,
                              colIndex = colIndex, rowIndex = rowIndex)
cameraDataSubset

## XLConnet package has more options for manipulating excel files


# Reading XML
library(XML)
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)

rootNode[[1]]

rootNode[[1]][[1]]

xmlSApply(rootNode, xmlValue)
xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)
##/node Top level node, //node Node at any level
##node[@attr-name] Nod with an attribute name
##node[@attr-name='bob'] Nod with attribute name attr-name='bob'

fileUrl <- "http://expn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc, "//li[@class='team-name']", xmlValue)


# Reading JSON
library(jsonlite)
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
jsonData$owner$login

myjson <- toJSON(iris, pretty = TRUE)
cat(myjson)
iris2 <- fromJSON(myjson)
head(iris2)


# Data Table
library(data.table)
DF = data.frame(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
head(DF,3)

DT = data.table(x = rnorm(9), y = rep(c("a", "b", "c"), each = 3), z = rnorm(9))
head(DT, 3)
tables()
DT[2,]
DT[DT$y == "a", ]
DT[c(2,3)]
DT[,c(2,3)]     ##passing value, not subsetting by column
DT[, list(mean(x), sum(z))]
DT[, table(y)]
DT[, w:=z^2]
DT2 <- DT
DT[, y:=2]

DT[, m:= {tmp <- (x + z); log2(tmp + 5)}]
DT[, a:= x > 0]
DT[, b:= mean(x + w), by = a]

set.seed(123)
DT <- data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[, .N, by=x]

DT <- data.table(x = rep(c("a", "b", "c"), each = 100), y = rnorm(300))
setkey(DT, x)
DT['a']

DT1 <- data.table(x = c('a', 'a', 'b', 'dt1'), y = 1: 4)
DT2 <- data.table(x = c('a', 'b', 'dt2'), z = 5:7)
setkey(DT1, x); setkey(DT2, x)
merge(DT1, DT2)

big_df <- data.frame(x = rnorm(1E6), y = rnorm(1E6))
file <- tempfile()
write.table(big_df, file = file, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
system.time(fread(file))
system.time(read.table(file, header = TRUE, sep = "\t"))


# Reading MySQL
library(RMySQL)
ucscDb <- dbConnect(MySQL(), user="genome", host="genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(ucscDb, "show databases;"); dbDisconnect(ucscDb)
result

hg19 <- dbConnect(MySQL(), user="genome", db="hg19", host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]

dbListFields(hg19, "affyU133Plus2")
dbGetQuery(hg19, "select count(*) from affyU133Plus2")

affyData <- dbReadTable(hg19, "affyU133Plus2")
head(affyData)
query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query); quantile(affyMis$misMatches)
affyMisSmall <- fetch(query, n=10); dbClearResult(query);
dim(affyMisSmall)
dbDisconnect(hg19)

# Reading HDF5
library(rhdf5)
source("http://bioconductor.org/biocLite.R")
##biocLite("rhdf5")
##just need to install once

library(rhdf5)
created = h5createFile("example.h5")
created = h5createGroup("example.h5", "foo")
created = h5createGroup("example.h5", "baa")
created = h5createGroup("example.h5", "foo/foobaa")
h5ls("example.h5")

A = matrix(1:10, nr=5, nc=2)
h5write(A, "example.h5", "foo/A")
B = array(seq(0.1, 2.0, by=0.1), dim = c(5,2,2))
attr(B, "scale") <- "liter"
h5write(B, "example.h5", "foo/foobaa/B")
h5ls("example.h5")

df = data.frame(1L:5L, seq(0,1,length.out=5),
                c("ab", "cde", "fghi", "a", "s"), stringAsFactors = FALSE)
h5write(df, "example.h5", "df")
h5ls("example.h5")

readA = h5read("example.h5", "foo/A")
readB = h5read("example.h5", "foo/foobaa/B")
readdf = h5read("example.h5", "df")
readA

h5write(c(12, 13,14), "example.h5", "foo/A", index = list(1:3, 1))
h5read("example.h5", "foo/A")

# Reading from the web
con = url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode = readLines(con)
close(con)
htmlCode

library(XML)
url <- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html <- htmlTreeParse(url, useInternalNodes=T)

xpathSApply(html, "//title", xmlValue)
xpathSApply(html, "//td[@id='col-citedby']", xmlValue)
# not getting correct result

library(httr); html2=GET(url)
content2 = content(html2, as="text")
parsedHtml = htmlParse(content2, asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)

pg1 = GET("http://httpbin.org/basic-auth/user/passwd")
pg1
pg2 = GET("http://httpbin.org/basic-auth/user/passwd",
          authenticate("user", "passwd"))
pg2
names(pg2)

google = handle("http://google.com")
pg1 = GET(handle=google, path="/")
pg2 = GET(handle=google, path="search")

# Reading from APIs
myapp = oauth_app("twitter", key = "P4G3KHRX5R02qXX4cl3XuedGR",
                  secret = "SEMMvUOvciFkNyGH72Web9TpARTe5pnBDH09Loq6H8CvwLf5qi")
sig = sign_oauth1.0(myapp, token = "146859214-mHTnroppUX7PSzfeR89wRBDZvjlDs2qPhvkEAlFo",
                    token_secret = "dvW2uQaY5QE3Ab7M2EXcoo8Q569ByQ1rRchRLVjPXNIg0")
##homeTL = GET("https://api.twitter.com/1.1/statuses/home_timeline.json", sig)
homeTL = GET("https://api.twitter.com/1.1/search/tweets.json", sig)

json1 = content(homeTL)
json2 = jsonlite::fromJSON(toJSON(json1))
json2[1, 1:4]


# Subsetting and Sorting
set.seed(13435)
X <- data.frame("var1" = sample(1:5), "var2" = sample(6:10), "var3" = sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)] = NA
X

X[,1]
X[,"var1"]
X[1:2, "var2"]
X[(X$var1 <= 3 & X$var3 > 11),]
X[(X$var1 <= 3 | X$var3 > 15),]
X[which(X$var2 > 8), ]

sort(X$var1)
sort(X$var1, decreasing = TRUE)
sort(X$var2, na.last = TRUE)

X[order(X$var1),]
X[order(X$var1, X$var3),]

library(plyr)
arrange(X, var1)
arrange(X, desc(var1))

X$var4 <- rnorm(5)
X

Y <- cbind(X, rnorm(5))
Y

# Summarzing Data
if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=Download"
download.file(fileUrl, destfile = "./data/restaurants.csv", method = "curl")
restData <- read.csv("./data/restaurants.csv")
head(restData, n = 3)
tail(restData, n = 3)
summary(restData)
str(restData)
quantile(restData$councilDistrict, na.rm = TRUE)
quantile(restData$councilDistrict, probs = c(0.5, 0.75, 0.9))

table(restData$zipCode, useNA = "ifany")
table(restData$councilDistrict, restData$zipCode)

sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode > 0)

colSums(is.na(restData))
all(colSums(is.na(restData)) == 0)

table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212", "21213"))
restData[restData$zipCode %in% c("21212", "21213"),]

data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)
summary(DF)

xt <- xtabs(Freq ~ Gender + Admit, data = DF)
xt

warpbreaks$replicate <- rep(1:9, len = 54)
xt = xtabs(breaks ~., data = warpbreaks)
xt
ftable(xt)

fakeData = rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData), units = "Mb")


# Creating New Variables
s1 <- seq(1, 10, by = 2); s1
s2 <- seq(1, 10 , length = 3); s2
x <- c(1, 3, 8, 25, 100); seq(along = x)

restData$nearMe = restData$neighborhood %in% c("Rolnad Park", "Homeland")
table(restData$nearMe)

restData$zipWrong = ifelse(restData$zipCode < 0, TRUE, FALSE)
table(restData$zipWrong, restData$zipCode < 0)

restData$zipGroups = cut(restData$zipCode, breaks = quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups, restData$zipCode)

library(Hmisc)
restData$zipGroups = cut2(restData$zipCode, g = 4)
table(restData$zipGroups)

restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)

yesno <- sample(c("yes", "no"), size = 10, replace = TRUE)
yesnofac = factor(yesno, levels = c("yes", "no"))
relevel(yesnofac, ref = "yes")
as.numeric(yesnofac)

library(Hmisc); library(plyr)
restData2 = mutate(restData, zipGroups = cut2(zipCode, g = 4))
table(restData2$zipGroups)

# Reshaping Data
library(reshape2)
head(mtcars)

mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id = c("carname", "gear", "cyl"), measure.vars = c("mpg", "hp"))
head(carMelt, n = 3)
tail(carMelt, n = 3)
cylData <- dcast(carMelt, cyl ~ variable)
cylData

cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData

head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns = split(InsectSprays$count, InsectSprays$spray)
spIns

sprCount = lapply(spIns, sum)
sprCount

unlist(sprCount)
sapply(spIns, sum)
ddply(InsectSprays, .(spray), summarize, sum = sum(count))

spraySums <- ddply(InsectSprays, .(spray), summarize, sum = ave(count, FUN = sum))
dim(spraySums)
head(spraySums)

# Managing Data Frames with dplyr
library(dplyr)
chicago <- readRDS("chicago.rds")
dim(chicago)
str(chicago)
names(chicago)
head(select(chicago, city:dptp))
head(select(chicago, -(city:dptp)))
i <- match("city", names(chicago))
j <- match("dptp", names(chicago))
head(chicago[, -(i:j)])
chic.f <- filter(chicago, pm25tmean2 > 30)
head(chic.f, 10)
chic.f <- filter(chicago, pm25tmean2 > 30 & tmpd > 80)
head(chic.f)

chicago <- arrange(chicago, date)
head(chicago)
tail(chicago)
chicago <- arrange(chicago, desc(date))
head(chicago)
tail(chicago)

chicago <- rename(chicago, pm25 = pm25tmean2, dewpoint = dptp)
head(chicago)

chicago <- mutate(chicago, pm25detrend = pm25 - mean(pm25, na.rm = TRUE))
head(select(chicago, pm25, pm25detrend))
chicago <- mutate(chicago, tempcat = factor(1 * (tmpd > 80), labels = c("cold", "hot")))
hotcold <- group_by(chicago, tempcat)
hotcold
summarize(hotcold, pm25 = mean(pm25), o3 = max(o3tmean2), no2 = median(no2tmean2))
summarize(hotcold, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

chicago <- mutate(chicago, year = as.POSIXlt(date)$year + 1900)
years <- group_by(chicago, year)
summarize(years, pm25 = mean(pm25, na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))
chicago %>% mutate(month = as.POSIXlt(date)$mon + 1) %>% group_by(month) %>% summarize(pm25 = mean(pm25, 
        na.rm = TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

# Merging Data
if(!file.exists("./data")) {dir.create("./data")}
fileUrl1 = "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 = "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "./data/reviews.csv", method = "curl")
download.file(fileUrl2, destfile = "./data/solutions.csv", method = "curl" )
reviews = read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews, 2)
head(solutions, 2)

names(reviews)
names(solutions)
mergedData =  merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)
head(mergedData)

intersect(names(solutions), names(reviews))
mergedData2 = merge(reviews, solutions, all = TRUE)
head(mergedData2)

df1 = data.frame(id = sample(1:10), x = rnorm(10))
df2 = data.frame(id = sample(1:10), y = rnorm(10))
arrange(join(df1, df2), id)

df3 = data.frame(id = sample(1:10), z = rnorm(10))
dfList = list(df1, df2, df3)
join_all(dfList)


# Learn from the peer review
## Checking for required packages
if (!("reshape2" %in% rownames(installed.packages())) ) {
        print("Installing required package \"reshape2\"")
        install.packages("reshape2")
} else {
        ## Open required libraries
        library(reshape2)
}

# Uses descriptive activity names to name the activities in the data set
data$activityName <- factor(data$activityId, labels =  activityLabel$activityName)

# Editing Text Variables
if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv", method = "curl")
cameraData <- read.csv("./data/cameras.csv")
names(cameraData)

tolower(names(cameraData))
splitNames = strsplit(names(cameraData), "\\.")
splitNames[[5]]
splitNames[[6]]

mylist <- list(letters = c("A", "b", "c"), number = 1:3, matrix(1:25, ncol = 5))
head(mylist)
mylist[1] 
letters
mylist$letters
mylist[[1]]

splitNames[[6]][1]
firstElement <- function(x) {x[1]}
sapply(splitNames, firstElement)
splitNames

fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1, destfile = "./data/reviews.csv", method = "curl")
download.file(fileUrl2, destfile = "./data/solutions.csv", method = "curl")
reviews <- read.csv("./data/reviews.csv"); solutions <- read.csv("./data/solutions.csv")
head(reviews, 2)
head(solutions, 2)
names(reviews)
sub("_", "", names(reviews),)
testName <- "this_is_a_test"
sub("_", "", testName)
gsub("_", "", testName)

grep("Alameda", cameraData$intersection)
table(grepl("Alameda", cameraData$intersection))
cameraData2 <- cameraData[!grepl("Alameda", cameraData$intersection),]
grep("Alameda", cameraData$intersection, value = TRUE)
grep("JeffStreet", cameraData$intersection)
length(grep("JeffStreet", cameraData$intersection))

library(stringr)
nchar("Jeffrey Leek")
substr("Jeffrey Leek", 1, 7)
paste("Jeffrey", "Leek")
paste0("Jeffrey", "Leek")
str_trim("Jeff    ")

# Working with Dates
d1 = date()
d1
class(d1)

d2 = Sys.Date()
d2
class(d2)
format(d2, "%a %b %d")

x = c("1jan1960", "2jan1960", "31mar1960", "30jul1960"); z = as.Date(x, "%d%b%Y")
z
z[1] - z[2]
as.numeric(z[1] - z[2])

weekdays(d2)
months(d2)
julian(d2)

library(lubridate); ymd("20140108")
mdy("08/04/2013")
dmy("03-04-2013")
ymd_hms("2011-08-03 10:15:13")
ymd_hms("2011-08-03 10:15:03", tz = "Pacific/Auckland")
?Sys.timezone

x = dmy(c("1jan2013", "2jan2013", "31mar2013", "30jul2013"))
wday(x[1])
wday(x[1], label = TRUE)
