## Either use source("./PrepareData.R") or below code

# Download the zip file by the provided URL and unzip it
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(dataUrl, "./NEI_data.zip", method = "curl")
unzip("./NEI_data.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset the NEI data by selecting data of Baltimore City
balNEI <- subset(NEI, fips == "24510")

# After some search around, I found it's not easy to determin which source is motor
# vehicle and I decided to find "motor" or "vehicle" in the EI.Sector column of SCC
# as the criteria
SCCMotor <- subset(SCC, grepl("motor|vehicle", EI.Sector, ignore.case = T))
SCCMotor$SCC <- as.character(SCCMotor$SCC)

# Check the column SCC from balNEI to the column of SCC from SCCMotor.
# The two data frames used below are from previous steps.
balNEIMotor <- subset(balNEI, balNEI$SCC %in% SCCMotor$SCC)

# Use tapply function to group by column year and perform the sum function
PM25byYear <- with(balNEIMotor, tapply(Emissions, year, sum))

# Plot the data without x-axis ticks first and then assign the ticks, add a line to
# make the trend more visible
plot(names(PM25byYear), PM25byYear, xlab = "Year", ylab = "Motor Vehicle Emissions in Baltimore (tons)",
     xaxt = "n")
axis(side = 1, at = names(PM25byYear))
lines(names(PM25byYear), PM25byYear, col = "red")
# The plot shows very clear that the emissions from motor vehicles kept decreasing over time

# Save the plot to a png file named "plot5.png"
dev.copy(png, file = "plot5.png")
dev.off()
