## Either use source("./PrepareData.R") or below code

# Download the zip file by the provided URL and unzip it
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(dataUrl, "./NEI_data.zip", method = "curl")
unzip("./NEI_data.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")

# Subset the data frame NEI by fips equals to 24510
balNEI <- subset(NEI, fips == "24510")
# Use tapply function to group by year and calculate the sum of emission
balPM25 <- with(balNEI, tapply(Emissions, year, sum))
# Plot the graph without x-axis ticks first and then assign the ticks based on
# the actual value by using names(totalPM25)
plot(names(balPM25), balPM25, xlab = "Year", ylab = "Total PM2.5 Emission in Baltimore City (tons)",
     xaxt = "n")
axis(side = 1, names(balPM25))
# Draw a line to make the trend more visible
lines(names(balPM25), balPM25, col = "red")
# The plot show emissions decreament from year 1999 to 2002, 2005 to 2008. However,
# there was increament from year 2002 to 2005.

# Copy the plot to a png file named "plot2.png"
dev.copy(png, file = "plot2.png")
dev.off()
