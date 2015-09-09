baltimoreNEI <- NEI[NEI$fips=="24510",]
aggTotalsBaltimore <- aggregate(Emissions ~ year, baltimoreNEI,sum)

barplot(
        aggTotalsBaltimore$Emissions,
        names.arg=aggTotalsBaltimore$year,
        xlab="Year",
        ylab="PM2.5 Emissions (Tons)",
        main="Total PM2.5 Emissions From All Baltimore City Sources"
)



library(ggplot2)
fileURL <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(fileURL, destfile = "./Project2.zip")
df <- unzip("./Project2.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

df <- subset(NEI, fips == "24510" | fips == "06037")

SCC$mv <- grepl("Onroad", SCC$Data.Category)
SCC2 <- subset(SCC, mv == TRUE)

keep_motor_vehicle <- merge(df, SCC2, by = "SCC")

df2 <- aggregate(Emissions ~ year + fips, data = keep_motor_vehicle, FUN = sum)
ggplot(data = df2, aes(x = year, y = Emissions, group = fips, color = fips)) + geom_line() + geom_point() + coord_cartesian(xlim = c(1998, 2009)) + scale_x_continuous(breaks = seq(1999, 2008, 3)) + xlab("Year") + ylab("Total Emissions") + ggtitle("Motor Vehicle-Related Emissions Comparison")

png("Motor_Vehicle_Related_Emissions_Comp.png")
print(ggplot(data = df2, aes(x = year, y = Emissions, group = fips, color = fips)) + geom_line() + geom_point() + coord_cartesian(xlim = c(1998, 2009)) + scale_x_continuous(breaks = seq(1999, 2008, 3)) + xlab("Year") + ylab("Total Emissions") + ggtitle("Motor Vehicle-Related Emissions Comparison"))
dev.off()