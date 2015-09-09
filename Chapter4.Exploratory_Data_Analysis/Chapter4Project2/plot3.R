## Either use source("./PrepareData.R") or below code

# Download the zip file by the provided URL and unzip it
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(dataUrl, "./NEI_data.zip", method = "curl")
unzip("./NEI_data.zip")

# Read data
NEI <- readRDS("summarySCC_PM25.rds")

if (require(dplyr) == F) {
        install.packages("dplyr")
}
library(dplyr)

# Subset the data frame NEI by fips equals to 24510, select columns "Emissions",
# "type" and "year"
balPM25 <- subset(NEI, fips == "24510", select = c("Emissions", "type", "year"))
# Use group_by function to group the data frame by type and year and then calculate the sum
# of the groups
grpBalPM25 <- group_by(balPM25, type, year)
sumBalPM25 <- summarise_each(grpBalPM25, funs(sum))

if (require(ggplot2) == F) {
        install.packages("ggplot2")
}
library(ggplot2)
# Use qplot from ggplot2 package to draw the plot, a smooth line connecting all the points
# added to show the trends.
qplot(year, Emissions, data = sumBalPM25, color = type, geom = c("point", "smooth"), method = "lm",
      xlab = "Year", ylab = "Total PM2.5 Emission in Baltimore City (tons)", facets = .~type)
# The plot shows very clear that emissions from NONPOINT, NON-ROAD and ON-ROAD decreases
# from 1999 to 2008, while emissions from POINT kept increasing from 1999 to 2005 and
# then decreased.

# Save the plot to a png file named "plot3.png"
dev.copy(png, width = 800, unit = "px", file = "plot3.png")
dev.off()
