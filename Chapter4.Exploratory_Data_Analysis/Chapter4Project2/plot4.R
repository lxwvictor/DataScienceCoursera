## Either use source("./PrepareData.R") or below code

# Download the zip file by the provided URL and unzip it
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(dataUrl, "./NEI_data.zip", method = "curl")
unzip("./NEI_data.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset the SCC data frame by searching "comb" and "coal" from the EI.Sector column
# i.e. in this case EI.Sector value contains "comb" and "coal" will be eligible
SCCCombCoal <- subset(SCC, grepl("comb.*coal", EI.Sector, ignore.case = T))

# Subset NEI data frame by checking SCC whose value is corresponding to coal combustion
PM25ByCoal <- subset(NEI, NEI$SCC %in% SCCCombCoal$SCC)
# Use tapply function to group the data by year
PM25ByCoalYear <- with(PM25ByCoal, tapply(Emissions, year, sum))

# Plot the data without x-axis ticks first and then assign the ticks, add a line to
# make the trend more visible
plot(names(PM25ByCoalYear), PM25ByCoalYear, xlab = "Year",
        ylab = "Total PM2.5 Emissions from Coal Combustion (tons)", xaxt = "n")
axis(side = 1, at = names(PM25ByCoalYear))
lines(names(PM25ByCoalYear), PM25ByCoalYear, col = "red")
# The plot shows very clear that the emissions from coal combustion-realted sources
# were relatively stable from 1999 to 2005, but decreased significantly from 2005 to 2008

# Save the plot to a png file named "plot4.png"
dev.copy(png, file = "plot4.png")
dev.off()
