if(!file.exists("./data")){dir.create("./data")}
fileurl<-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileurl, destfile="./data/restaurants.csv" method="curl")
restData <- read.table("./data/restaurants.csv")

head(restData,n=3)
tail(restData,n=3)
summary(restData)
str(restData) 
quantile(restData$councilDistrict, na.rm=TRUE)
table(restData$zipCode, useNA="ifany")
sum(is.na(restData$councilDistrict))
colSums(is.na(restData))

table(restData$zipCode  %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))

data(UCBAdmissions)
DF=as.data.frame(UCBAdmissions)
summary(DF) 
xt <- xtabs(Freq ~ Gender +Admit, data=DF)
xt
yt<- xtabs(Freq ~., data=DF) 
ft<- ftable(yt)
ft

fakeData =rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units="Mb")

S1<- seq(1,10,by=2); S1
S2 <-seq(1,10,length=3); S2
restData$zipwrong=ifelse(restData$zipCode<0, TRUE, FALSE) 
restData$zipwrong
table(restData$zipwrong, restData$zipCode<0)

restData$zipGroups = cut(restData$zipCode, breaks=quantile(restData$zipCode))
restData$zipGroups
table(restData$zipGroups)   

restData$zcf <- factor(restData$zipCode)   
restData$zcf[1:10]  

Install.packages("Hmisc")
library(HMisc); library(plyr)
restData2 <- mutate(restData, zipGroups=cut2(zipCode,g=4))
table(restData2$zipGroups)

library(reshape2)
head(mtcars)    
mtcars$carname<-rownames(mtcars)
carMelt <-melt(mtcars, id=c("carname","gear","cyl"), measure.vars=c("mpg","hp"))
head(carMelt,n=3)

cylData<-dcast(carMelt, cyl ~variable,mean)
cylData

head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray,sum) 

spIns = split(InsectSprays$count, InsectSprays$spray)
spIns
sprCount = lapply(spIns, sum)          
sprCount
unlist(sprCount)


if(!file.exists("./data")){dir.create("./data")}
fileurl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileurl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileurl1, destfile=¡±./data/reviews.csv¡±, method=¡±curl¡±)
download.file(fileurl2, destfile="./data/solutions.csv", method="curl")
reviews <- read.csv("./data/reviews-apr29.csv")
solutions <- read.csv("./data/solutions-apr29.csv"); 
names(reviews)
names(solutions)

mergeData= merge(reviews, solutions, by.x="solution_id", by.y="id",all=TRUE)
head(mergeData)      
intersect(names(solutions), names(reviews))
mergeData2= merge(reviews, solutions, all=TRUE)
head(mergeData2)     
df1 = data.frame(id=sample(1:10), x=rnorm(10))
df2 = data.frame(id=sample(1:10), y=rnorm(10))
df3 = data.frame(id=sample(1:10), z=rnorm(10))
dfList = list(df1,df2,df3)
join_all(dfList)

fileurl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileurl, destfile="./data/cameras.csv", method="curl")
cameraData <- read.csv("./data/cameras.csv")
names(cameraData)

tolower(names(cameraData))  
splitNames = strsplit(names(cameraData),"\\.")
splitNames[[6]]        
firstElement <- function(x){x[1]}
sapply(splitnames,firstElement)    
testName <- "this_is_a_test"
sub("_","",testName)
gsub("_","",testName)
grep("Alameda",cameraData$intersection)
grep("Alameda",cameraData$intersection, value=TRUE)
table(grepl("Alameda",cameraData$intersection))

nchar("This is a test")
substr("Getting Data",1,7)
paste("Getting","Data")
paste0("Getting","Data")
library(stringr)
str_trim(" Getting and Cleaning Date       ")

d2=Sys.Date(); d2
format(d2,"%a %b %d")
x=c("1Jan2014","5Feb2015")
z=as.Date(x,"%d%b%Y")
z[1]-z[2]
julian(d2)

library(lubridate) 
ymd("20150108")
ymd_hms("2015-02-13 10:34:09")
