## Either use source("./PrepareData.R") or below code

# Download the zip file by the provided URL and unzip it
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(dataUrl, "./NEI_data.zip", method = "curl")
unzip("./NEI_data.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")

# Use tapply to group the data by year to calculate the sum
totalPM25 <- with(NEI, tapply(Emissions, year, sum))
# Plot the graph without x-axis ticks first and then assign the ticks based on
# the actual value by using names(totalPM25)
plot(names(totalPM25), totalPM25, xlab = "Year", ylab = "Total PM2.5 Emission (tons)",
     xaxt = "n", )
axis(side = 1, at = names(totalPM25))
# Draw a line to make the trend more visible
lines(names(totalPM25), totalPM25, col = "red")
# The plot shows the total emissions kept decreasing, especially for year 1999 to 2002
# and year 2005 to 2008.

# Copy the plot to png file named "plot1.png"
dev.copy(png, file = "plot1.png")
dev.off()
