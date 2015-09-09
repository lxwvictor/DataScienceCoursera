#if(require(R.utils) == F) install.packages("R.utils")
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileUrl, destfile = "./repdata-data-StormData.csv.bz2", method = "curl")
# bunzip2("./repdata-data-StormData.csv.bz2", overwrite = TRUE)
# storm <- read.csv("./repdata-data-StormData.csv", header = T, stringsAsFactors = F)
storm <- read.csv(bzfile("./repdata-data-StormData.csv.bz2"), header = T, stringsAsFactors = F)


intData2$EVTYPE[grep("Flood|Dam Break|RIP Current|Astronomical|Stream|Urban small", intData2$EVTYPE, ignore.case = T)] <- "Flood"
intData2$EVTYPE[grep("Tornado|Torndao|Funnel Cloud|Waterspout|Landspout", intData2$EVTYPE, ignore.case = T)] <- "Tornado"
intData2$EVTYPE[grep("Snow|Blizzard", intData2$EVTYPE, ignore.case = T)] <- "Snow"
intData2$EVTYPE[grep("Hail|Ice Storm", intData2$EVTYPE, ignore.case = T)] <- "Hail"
intData2$EVTYPE[grep("Erosion", intData2$EVTYPE, ignore.case = T)] <- "Erosion"
intData2$EVTYPE[grep("Marine|Surf|Wave|Swell|Tide|Coastal|Storm Surge|High Water|Seiche", intData2$EVTYPE, ignore.case = T)] <- "Wave"
intData2$EVTYPE[grep("Wind|TSTM|Turbulence|Gustnado|Apache", intData2$EVTYPE, ignore.case = T)] <- "Wind"
intData2$EVTYPE[grep("Dust", intData2$EVTYPE, ignore.case = T)] <- "Dust"
intData2$EVTYPE[grep("Rain|Precipitation|Shower|Urban and small", intData2$EVTYPE, ignore.case = T)] <- "Rain"
intData2$EVTYPE[grep("Storm", intData2$EVTYPE, ignore.case = T)] <- "Storm"
intData2$EVTYPE[grep("Fire", intData2$EVTYPE, ignore.case = T)] <- "Fire"
intData2$EVTYPE[grep("Frost", intData2$EVTYPE, ignore.case = T)] <- "Frost"
intData2$EVTYPE[grep("Heat|Warm", intData2$EVTYPE, ignore.case = T)] <- "Heat"
intData2$EVTYPE[grep("Cold", intData2$EVTYPE, ignore.case = T)] <- "Cold"
intData2$EVTYPE[grep("Wet", intData2$EVTYPE, ignore.case = T)] <- "Wet"
intData2$EVTYPE[grep("Drought", intData2$EVTYPE, ignore.case = T)] <- "Drought"
intData2$EVTYPE[grep("Fog", intData2$EVTYPE, ignore.case = T)] <- "Fog"
intData2$EVTYPE[grep("Freeze|Freezing", intData2$EVTYPE, ignore.case = T)] <- "Freeze"
intData2$EVTYPE[grep("Hurricane", intData2$EVTYPE, ignore.case = T)] <- "Hurricane"
intData2$EVTYPE[grep("Ice|Icy|Glaze", intData2$EVTYPE, ignore.case = T)] <- "Ice"
intData2$EVTYPE[grep("Typhoon|Depression", intData2$EVTYPE, ignore.case = T)] <- "Typhoon"
intData2$EVTYPE[grep("Waterspout", intData2$EVTYPE, ignore.case = T)] <- "Waterspout"
intData2$EVTYPE[grep("Winter|Wintry", intData2$EVTYPE, ignore.case = T)] <- "Winter"
intData2$EVTYPE[grep("Smoke|Ash", intData2$EVTYPE, ignore.case = T)] <- "Smoke"
intData2$EVTYPE[grep("Ligntning|Lightning|Lighting", intData2$EVTYPE, ignore.case = T)] <- "Lightning"
intData2$EVTYPE[grep("Mud", intData2$EVTYPE, ignore.case = T)] <- "Mudslide"
intData2$EVTYPE[grep("Burst", intData2$EVTYPE, ignore.case = T)] <- "Burst"
intData2$EVTYPE[grep("Landslide", intData2$EVTYPE, ignore.case = T)] <- "Landslide"
intData2$EVTYPE[grep("\\?|other", intData2$EVTYPE, ignore.case = T)] <- "Other"


ggplot(data = economic, aes(x = EVTYPE, y = COST, colour = EVTYPE, xlab = "Event Type")) + geom_bar(aes(fill = EVTYPE),stat="identity")