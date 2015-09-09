download.file("http://www.socuteurl.com/poofyfuzzydoggy",dest="./therbook.zip", method = "curl")
unzip("therbook.zip")

# Boxplots
weather = read.table("./therbook/SilwoodWeather.txt", h = T)
onemonth = subset(weather, month == 1 & yr == 2004)
boxplot(onemonth$rain)

# Histograms
hist(weather$upper)
rug(weather$upper)

# Barplot
barplot(table(weather$month),
        col = "wheat",
        main = "Number of Observations in Months")

# Base plots: Scatterplot
data1 = read.table("./therbook/Scatter1.txt", h = T)
data2 = read.table("./therbook/Scatter2.txt", h = T)
with(data1, plot(xv, ys, col = "red"))
with(data1, abline(lm(ys ~ xv)))
with(data2, points(xv2, ys2, col = "blue", pch = 11))   # needs to use points instead of plot, else redraw
title("My Title", oouter = TRUE)

par(mfrow = c(1,2))
with(data1, plot(xv, ys, col = "red"))
with(data1, abline(lm(ys ~ xv)))
with(data2, point(xv2, ys2, col = "blue", pch = 11))
title("My Title", outer = TRUE)

mfrow(mar = c(5.1, 4.1, 4.1, 2.1), oma = c(2, 2, 2, 2))

# Lattice
productivity = read.table("./therbook/productivity.txt", h = T)
library(lattice)
xyplot(x ~ y, productivity, xlab = list(label = "Productivity"), ylab = list(label = "Mammal Species"))
xyplot(x ~ y | f, productivity, xlab = list(label = "Productivity"), ylab = list(label = "Mammal Species"))

# ggplot
library(ggplot2)
library(dplyr)
weather <- transform(weather, month = as.factor(month))
ggplot(weather, aes(x = month, y = upper)) + geom_boxplot()
weather2 = weather %>%
        group_by(month) %>%
        summarise(average.upper = mean(upper))

ggplot(weather2, aes(month, average.upper)) + 
        geom_bar(stat = "identity")

ggplot(weather2, aes(month, average.upper)) + 
        geom_bar(aes(fill = month), stat = "identity") +
        scale_fill_brewer(palette = "Set3") +
        xlab("Months") +
        ylab("Upper Quantile") + theme_bw()

qplot(month, upper, fill = month, data = weather, facets = ~yr, geom = "bar", stat = "identity")

data = read.csv("./therbook/final.csv")
areas = unique(subset(data, select = c(Planning_Area, Planning_Region)))
areas = areas[order(areas$Planning_Region),]
areas$rectid = 1:nrow(areas)
rectdata = areas %>% group_by(Planning_Region) %>%
        summarise(xstat = min(rectid) - 0.5, xend = max(rectid) + 0.5)

data$Planning_Area = factor(data$Planning_Area, leves = 
                                    as.character(areas[order(areas$Planning_Region),]$Planning_Area))