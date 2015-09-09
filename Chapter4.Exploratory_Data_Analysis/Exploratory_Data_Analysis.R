setwd("./Chapter4.Exploratory_Data_Analysis/")
# check whether the "ggplot2" package has instaleld
if(require(ggplot2) == F) {
        install.packages("ggplot2")
}
library(ggplot2)

## Exploratory Graphs
pollution <- read.csv("PM25data/avgpm25.csv", colClasses = c("numeric",
        "character", "factor", "numeric", "numeric"))
head(pollution)

summary(pollution$pm25)
boxplot(pollution$pm25, col = "blue")
hist(pollution$pm25, col = "green")
rug(pollution$pm25)

hist(pollution$pm25, col = "green", breaks = 100)
rug(pollution$pm25)

boxplot(pollution$pm25, col = "blue")
abline(h = 12)

hist(pollution$pm25, col = "green")
abline(v = 12, lwd = 2)
abline(v = median(pollution$pm25), col = "magenta", lwd = 4)
# lwd mean line width

barplot(table(pollution$region), col = "wheat", main = "Number of Counties in Each Region")

boxplot(pm25 ~ region, data = pollution, col = "red")

par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
# "mfrow" vector means graphs in 2 rows, 1 column
# "mar" vector gives the number of lines of margin of the 4 sides, c(bottom, left, top, right)
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
# Above 2 lines needs the RStudio window big, else can't plot

with(pollution, plot(latitude, pm25, col = region))
abline(h = 12, lwd = 2, lty = 2)
# The "with(pollution, plot(latitude, pm25))" is equivalant to below
plot(pollution$latitude, pollution$pm25)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 1))
with(subset(pollution, region = "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region = "east"), plot(latitude, pm25, main = "East"))
# "main" means overall title for the plot

## Plotting Systems
# Base Plot
library(datasets)
data(cars)
with(cars, plot(speed, dist))

# The Lattice System
library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

# The ggplot2 System
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

## Base Plotting System
library(datasets)
hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone))
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)")

par("lty")
par("col")
par("pch")
par("bg")
par("mar")
par("mfrow")

library(datasets)
with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",
        type = "n"))
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue", "red"), legend = c("May", "Other Months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in New York City",
        pch = 20))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

par(mfrow = c(1, 2))
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, main = "Ozone and Solar Radiation")
})

par(mfrow = c(1, 3), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
with(airquality, {
        plot(Wind, Ozone, main = "Ozone and Wind")
        plot(Solar.R, Ozone, mian = "Ozone and Solar Radiation")
        plot(Temp, Ozone, main = "Ozone and Temperature")
        mtext("Ozone and Weather in New York City", outer = TRUE)
})

## Base plotting demonstration
x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x, y)
z <- rnorm(100)
plot(x, z)
plot(x, y)
par(mar = c(2, 2, 2, 2))
plot(x, y)
par(mar = c(4, 4, 2, 2))
plot(x, y)
plot(x, y, pch = 20)
plot(x, y, pch = 19)
plot(x, y, pch = 2)
plot(x, y, pch = 3)
plot(x, y, pch = 4)
example(points)
title("Scatterplot")
text(-2, -2, "Label")
legend("topleft", legend = "Data")
legend("topleft", legend = "Data", pch = 20)
fit <- lm(y ~ x)
abline(fit)
abline(fit, lwd = 3)
abline(fit, lwd = 3, col = "blue")
plot(x, y, xlab = "Weight", ylab = "Height", main = "Scatterplot", pch = 20)
fit <- lm(y ~ x)
abline(fit, lwd =  3 , col = "red")
z <- rpois(100, 2)
par(mfrow = c(2, 1))
plot(x, y, pch = 20)
plot(x, z, pch = 19)
par("mar")
par(mar = c(2, 2, 1, 1))
plot(x, y, pch = 20)
plot(x, z, pch = 19)
par(mfrow = c(1, 2))
plot(x, y, pch = 20)
plot(x, z, pch = 19)
par(mfrow  = c(2,2))
plot(x, y)
plot(x, z)
plot(z, x)
plot(z, y)
par(mfcol = c(2, 2))
plot(x, y)
plot(x, z)
plot(z, x)
plot(z, y)
par(mfrow = c(1, 1))
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50)
g <- gl(2, 50, labels = c("Male", "Female"))
str(g)
plot(x, y)
plot(x, y, type = "n")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g == "Female"], col = "blue")
points(x[g == "Female"], y[g == "Female"], col = "blue", pch = 19)


## Graphics Devices in R
library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser, data")

pdf(file = "myplot.pdf")
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser data")
dev.off()

dev.cur()

library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = "old Failthful Geyser data")
dev.copy(png, file = "geyserplot.png")
dev.off()


## Learn from peers
mutate(DateTime = parse_date_time(DateTime, orders = "%d/%m*/%Y %H:%M:%s"))

## The Lattice Plotting System in R
library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data = airquality)

# Conver 'Mont' to a factor variable
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))

p <- xyplot(Ozone ~ Wind, data = airquality)
print(p)
xyplot(Ozone ~ Wind, data = airquality)

set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2, 1))

xyplot(y ~ x | f, panel = function(x, y, ...) {
        panel.xyplot(x, y, ...)
        panel.abline(h = median(y), lty = 2)
        panel.lmline(x, y, col = 2)
})

## The ggplot2 Plotting System
library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, col = drv)
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))
qplot(hwy, data = mpg, fill = drv)
qplot(displ, hwy, data = mpg, facets = .~drv)
qplot(hwy, data = mpg, facets = drv~., binwidth =2)

load("./maacs.rda")
str(maacs)
qplot(log(eno), data = maacs)
qplot(log(eno), data = maacs, fill = mopos)
qplot(log(eno), data = maacs, geom = "density")
qplot(log(eno), data = maacs, geom = "density", color = mopos)
qplot(log(pm25), log(eno), data = maacs)
qplot(log(pm25), log(eno), data = maacs, shape = mopos)
qplot(log(pm25), log(eno), data = maacs, color = mopos)
qplot(log(pm25), log(eno), data = maacs, color = mopos, geom = c("point", "smooth"),
      method = "lm")
qplot(log(pm25), log(eno), data = maacs, geom = c("point", "smooth"), method = "lm",
      facets = .~mopos)

testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50, 2] <- 100
plot(testdat$x, testdat$y, type = "l", ylim = c(-3, 3))

g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line()

g + geom_line() + ylim(-3, 3)
g + geom_line() + coord_cartesian(ylim = c(-3, 3))

ggplot(mpg, aes(x = hwy, y = cty)) + geom_point(aes(size = cyl), colour = "red")
ggplot(mpg, aes(x = hwy)) + geom_density(fill = "steelblue")
ggplot(mpg, aes(x = hwy)) + geom_histogram(binwidth = 2, fill = "steelblue")
ggplot(mpg, aes(x = factor(cyl), y = hwy)) + geom_boxplot(fill = "steelblue")

ggplot(mtcars, aes(factor(am))) + geom_bar(fill = "steelblue") +
        scale_x_discrete(breaks = c(0, 1), labels = c("Automatic", "Manual")) +
        xlab("Transmission Type")

ggplot(mpg, aes(x = hwy, y = cty)) + geom_point(colour = "red")

ggplot(mpg, aes(x = hwy, y = cty)) + geom_point(aes(size = cyl), colour = "red") +
        scale_size(name = "No. of\nCylinders", range = c(2, 6)) +
        xlab("Highway miles/gallon") + 
        ylab("City mies/gallon") + facet_wrap(~class) +
        theme(axis.text = element_text(size = 15, colour = "black"),
        axis.title = element_text(size = 20),
        strip.text.x = element_text(size = 15),
        legend.title = element_text(size = 15))

ggplot(mpg, aes(x = hwy, y = cty)) + geom_point(aes(size = cyl), colour = "red") +
        scale_size(name = "No. of\nCylinders", range = c(2, 6)) +
        xlab("Highway miles/gallon") + 
        ylab("City mies/gallon") + geom_smooth() +
        theme(axis.text = element_text(size = 15, colour = "black"),
              axis.title = element_text(size = 20),
              strip.text.x = element_text(size = 15),
              legend.title = element_text(size = 15))

dist1 <- dist(mtcars)
hclust_model <- hclust(dist1)
plot(hclust_model)
rect.hclust(hclust_model, k = 3, border = "red")

fit <- cmdscale(dist1)
plot(fit, type = 'n')
text(fit, rownames(fit))

set.seed(123)
kmeans_model <- kmeans(mtcars, centers = 3)
str(kmeans_model)
pairs(mtcars, col = kmeans_model$cluster)

PCA <- prcomp(mtcars, scale = T)
biplot(PCA, cex = 0.7)


## Hierarchical Clustering
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x = x, y =y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)), hang = 0.1, ...) {
        ## modifiction of plclust for plotting hclust objects *in colour*! Copyright
        ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
        ## of labels of the leaves of the tree lab.col: colour for the labels;
        ## NA=default device foreground colour hang: as in hclust & plclust Side
        ## effect: A display of hierarchical cluster with coloured leaf labels.
        y <- rep(hclust$height, 2)
        x <- as.numeric(hclust$merge)
        y <- y[which(x < 0)]
        x <- x[which(x < 0)]
        x <- abs(x)
        y <- y[order(x)]
        x <- x[order(x)]
        plot(hclust, labels = FALSE, hang = hang, ...)
        text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order],
             col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))

dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)


## K-means Clustering
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster
kmeansObj$centers

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")


## Principal Components Analysis and Singular Value Decomposition
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)

set.seed(678910)
for(i in 1:40) {
        # flip a coin
        coinFlip <- rbinom(1, size = 1, prob = 0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
        }
}
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
heatmap(dataMatrix)

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector",
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1",
     ylab = "Right Singular Vector 1")
abline(c(0, 1))

constantMatrix <- dataMatrixOrdered * 0
for(i in 1:dim(dataMatrixOrdered)[1]) {constantMatrix[i, ] <- rep(c(0, 1), each = 5)}
svd1 <- svd(constantMatrix)
par(mfrow = c(1, 3))
image(t(constantMatrix)[, nrow(constantMatrix):1])
plot(svd1$d, xlab = "column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)

set.seed(678910)
for (i in 1:40) {
        # flip a coin
        coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
        coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
        # if coin is heads add a common pattern to that row
        if (coinFlip1) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
        }
        if (coinFlip2) {
                dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
        }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered) [, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered) [, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained",
     pch = 19)

dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))
# error due to missing value

if (require(impute) == F) install.packages("impute")
library(impute)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow = c(1, 2)); plot(svd1$v[, 1], pch = 19); plot(svd2$v[, 1], pch = 19)

load("./data/face.rda")
image(t(faceData) [, nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")

svd1 <- svd(scale(faceData))
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1]
approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[, 1:5])
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10])
par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image(t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(faceData)[, nrow(faceData):1], main = "(d)")


## Plotting and Color in R
install.packages("grDevices")
pal <- colorRamp(c("red", "blue"))
pal(0)
pal(1)
pal(0.5)

pal(seq(0, 1, len = 10))

pal <- colorRampPalette(c("red", "yellow"))
pal(2)
pal(10)

install.packages("RColorBrewer")
library(RColorBrewer)
cols <- brewer.pal(3, "BuGn")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

x <- rnorm(1000)
y <- rnorm(1000)
plot(x, y, pch = 19)
plot(x, y, col = rgb(0, 0, 0, 0.2), pch = 19)


## Clustering Case Study
load("data/samsungData.rda")
names(samsungData)[1:12]
table(samsungData$activity)

par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
samsungData <- transform(samsungData, activity = factor(activity))
sub1 <- subset(samsungData, subject == 1)
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1)[1])
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity),
       pch = 1)

source("myplclust.R")
distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))

par(mfrow = c(1, 2))
plot(sub1[, 10], pch = 19, col = sub1$activity, ylab = names(sub1)[10])
plot(sub1[, 11], pch = 19, col = sub1$activity, ylab = names(sub1)[11])

svd1 = svd(scale(sub1[, -c(562, 563)]))
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = sub1$activity, pch = 19)
plot(svd1$u[, 2], col = sub1$activity, pch = 19)

plot(svd1$v[, 2], pch = 19)
maxContrib <- which.max(svd1$v[, 2])
distanceMatrix <- dist(sub1[, c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activitiy))

names(samsungData)[maxContrib]

kClust <- kmeans(sub1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, sub1$activity)

plot(kClust$center[1, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")


## Air Pollution Case Study
## Has fine particle pollution in the U.S. decreased from 1999 to
## 2012?

## Read in data from 1999

pm0 <- read.table("./pm25_data/RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
dim(pm0)
head(pm0)
cnames <- readLines("./pm25_data/RD_501_88101_1999-0.txt", 1)
print(cnames)
cnames <- strsplit(cnames, "|", fixed = TRUE)
print(cnames)
names(pm0) <- make.names(cnames[[1]])
head(pm0)
x0 <- pm0$Sample.Value
class(x0)
str(x0)
summary(x0)
mean(is.na(x0))  ## Are missing values important here?

## Read in data from 2012

pm1 <- read.table("./pm25_data/RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "", nrow = 1304290)
names(pm1) <- make.names(cnames[[1]])
head(pm1)
dim(pm1)
x1 <- pm1$Sample.Value
class(x1)

## Five number summaries for both periods
summary(x1)
summary(x0)
mean(is.na(x1))  ## Are missing values important here?

## Make a boxplot of both 1999 and 2012
boxplot(x0, x1)
boxplot(log10(x0), log10(x1))

## Check negative values in 'x1'
summary(x1)
negative <- x1 < 0
sum(negative, na.rm = T)
mean(negative, na.rm = T)
dates <- pm1$Date
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month")  ## Check what's going on in months 1--6


## Plot a subset for one monitor at both times

## Find a monitor for New York State that exists in both datasets
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
site0 <- paste(site0[,1], site0[,2], sep = ".")
site1 <- paste(site1[,1], site1[,2], sep = ".")
str(site0)
str(site1)
both <- intersect(site0, site1)
print(both)

## Find how many observations available at each monitor
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep = "."))
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)

## Choose county 63 and side ID 2008
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dim(pm1sub)
dim(pm0sub)

## Plot data for 2012
dates1 <- pm1sub$Date
x1sub <- pm1sub$Sample.Value
plot(dates1, x1sub)
dates1 <- as.Date(as.character(dates1), "%Y%m%d")
str(dates1)
plot(dates1, x1sub)

## Plot data for 1999
dates0 <- pm0sub$Date
dates0 <- as.Date(as.character(dates0), "%Y%m%d")
x0sub <- pm0sub$Sample.Value
plot(dates0, x0sub)

## Plot data for both years in same panel
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20)  ## Whoa! Different ranges
abline(h = median(x1sub, na.rm = T))

## Find global range
rng <- range(x0sub, x1sub, na.rm = T)
rng
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm = T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = T))

## Show state-wide means and make a plot showing trend
head(pm0)
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn0)
summary(mn0)
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = T))
str(mn1)

## Make separate data frames for states / years
d0 <- data.frame(state = names(mn0), mean = mn0)
d1 <- data.frame(state = names(mn1), mean = mn1)
mrg <- merge(d0, d1, by = "state")
dim(mrg)
head(mrg)

## Connect lines
par(mfrow = c(1, 1))
with(mrg, plot(rep(1, 52), mrg[, 2], xlim = c(.5, 2.5)))
with(mrg, points(rep(2, 52), mrg[, 3]))
segments(rep(1, 52), mrg[, 2], rep(2, 52), mrg[, 3])