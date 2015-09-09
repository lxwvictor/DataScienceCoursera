setwd("./Chapter7.Regression_Models/")
## Introdcution to regression
if (require(UsingR) == FALSE)   install.packages("UsingR")
if (require(ggplot2) == FALSE)   install.packages("ggplot2")
library(UsingR)
library(ggplot2)

data(galton)
par(mfrow = c(1,2))
hist(galton$child, col = "blue", breaks = 100)
hist(galton$parent, col = "blue", breaks = 100)

if (require(manipulate) == FALSE)  install.packages("manipulate")
library(manipulate)
myHist <- function(mu) {
        hist(galton$child, col = "blue", breaks = 100)
        lines(c(mu, mu), c(0, 150), col = "red", lwd = 5)
        mse <- mean((galton$child - mu) ^ 2)
        text(63, 150, paste("mu = ", mu))
        text(63, 140, paste("MSE = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

hist(galton$child, col = "blue", breaks = 100)
meanChild <- mean(galton$child)
lines(rep(meanChild, 100), seq(0, 150, length = 100), col = "red", lwd = 5)

# Comparing childrens' heights and their parents' heights
plot(galton$parent, galton$child, pch = 19, col = "blue")

# Size of point represents number of points
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq, 
     xlab = "parent", ylab = "child")

# Regression through the origin
myPlot <- function(beta) {
        y <- galton$child - mean(galton$child)
        x <- galton$parent - mean(galton$parent)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("child", "parent", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)),
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq,
                xlab = "parent",
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean((y - beta * x)^2)
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

# Introductory Data Example
y <- galton$child - mean(galton$child)
x <- galton$parent - mean(galton$parent)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
myPlot <- function(beta){
        g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
        g <- g  + scale_size(range = c(2, 20), guide = "none" )
        g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
        g <- g + geom_point(aes(colour=freq, size = freq))
        g <- g + scale_colour_gradient(low = "lightblue", high="white")                     
        g <- g + geom_abline(intercept = 0, slope = beta, size = 3)
        mse <- mean( (y - beta * x) ^2 )
        g <- g + ggtitle(paste("beta = ", beta, "mse = ", round(mse, 3)))
        g
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

# The solution of best fit
lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)

# Visual the best fit line, the first way
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")                    
lm1 <- lm(galton$child ~ galton$parent)
g <- g + geom_abline(intercept = coef(lm1)[1], slope = coef(lm1)[2], size = 3, colour = grey(.5))
g

# Visualize the best fit line, the second way
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .05 * freqData$freq, 
     xlab = "parent", ylab = "child")
lm1 <- lm(galton$child ~ galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)


## Line least square coding example
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
rbind(c(beta0, beta1), coef(lm(y ~ x)))

# Center at mean, slope is the same
yc <- y - mean(y)
xc <- x - mean(x)
beta1 <- sum(yc * xc) / sum(xc ^ 2)
c(beta1, coef(lm(y ~ x))[2])
lm(yc ~ xc - 1)

# Normalized variables, slopes are the same
yn <- (y - mean(y)) / sd(y)
xn <- (x - mean(x)) / sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])

if (require(dplyr) == FALSE) install.packages("dplyr")
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g + scale_size(range = c(2, 20), guide = "none")
g <- g + geom_point(colour = "grey50", aes(size = freq + 20, show_guide = FALSE))
g <- g + geom_point(aes(colour = freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high = "white") 
g <- g + geom_smooth(method = "lm", formula = y ~ x, group = 1)
g


## Regression to the mean
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 4, colour = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1/rho, size = 2)
g


## Statistical Linear Regression Models
library(UsingR)
data("diamond")
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price),)
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g = g + geom_smooth(method = "lm", color = "black")
g

fit <- lm(price ~ carat, data = diamond)
coef(fit)

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)

fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)

# Predicting the price
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
predict(fit, newdata = data.frame(carat = newx))

## Residuals
library(UsingR)
data("diamond")
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price),)
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, colour = "black", alpha = 0.2)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g = g + geom_smooth(method = "lm", color = "black")
g

# Residuals, coding example
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(fit, lwd = 2)
for (i in 1:n)
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)

plot(x, e,
     xlab = "Mass (carats)",
     ylab = "Residuals (SIN $)",
     bg = "lightblue",
     col = "black", cex = 2, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1:n)
        lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)

# Non-linear data
x = runif(100, -3, 3); y = x + sin(x) + rnorm(100, sd = .2);
library(ggplot2)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

# Residual plot
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2);
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g = g + xlab("x") + ylab("Residual")
g

# Heteroscedasticity
x <- runif(100, 0, 6); y <- x + rnorm(100, mean = 0, sd = .001 * x)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", colour = "black")
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.4)
g = g + geom_point(size = 5, colour = "red", alpha = 0.4)
g

diamond$e <- resid(lm(price ~ carat, data = diamond))
g = ggplot(diamond, aes(x = carat, y = e))
g = g + xlab("Mass (carats)")
g = g + ylab("Residual price (SIN $)")
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, colour = "black", alpha = 0.5)
g = g + geom_point(size = 5, colour = "blue", alpha = 0.2)
g

# Diamond data residual plot
e = c(resid(lm(price ~ 1, data = diamond)),
      resid(lm(price ~ carat, data = diamond)))
fit = factor(c(rep("Itc", nrow(diamond)),
               rep("Itc, slope", nrow(diamond))))
g = gglot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 20)
g = g + xlab("Fitting approch")
g = g + ylab("Residual price")
g

## Residual variance
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
summary(fit)
summary(fit)$sigma
sqrt(sum(resid(fit) ^ 2) / (n - 2))

data("anscombe")
example("anscombe")

## Inference in Regression
library(UsingR); data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y)/sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e^2) / (n-2)) 
ssx <- sum((x - mean(x))^2)
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0; tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable

fit <- lm(y ~ x)
summary(fit)$coefficients

sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]) / 10

## Predication
library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2) 
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g

## Multivariable Regression
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3))

library(datasets); data(swiss); require(stats); require(graphics)
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))
summary(lm(Fertility ~ ., data = swiss))
summary(lm(Fertility ~ Agriculture, data = swiss)) $coefficients

n <- 100; x2 <- 1:n; x1 <- .01 * 2 + runif(n , -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)

## Dummy Variables
summary(lm(count ~ spray, data = InsectSprays))$coef
summary(lm(count ~
                   I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
                   I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
                   I(1 * (spray == 'F')), data = InsectSprays))$coef

# if include all 6
summary(lm(count ~
                   I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
                   I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
                   I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))

# omit intercept
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
unique(ave(InsectSprays$count, InsectSprays$spray))

spray2 <- relevel(InsectSprays$spray, "C") # C is ref
summary(lm(count ~ spray2, data = InsectSprays))$coef

fit <- lm(count ~ spray, data = InsectSprays) # A is ref
bbmbc <- coef(fit)[2] - coef(fit)[3] # B - C
temp <- summary(fit)
se <- temp$sigma * sqrt(temp$cov.unscaled[2, 2] + temp$cov.unscaled[3, 3] - 2 * temp$cov.unscaled[2, 3])
t <- (bbmbc) / se
p <- pt(-abs(t), df = fit$df)
out <- c(bbmbc, se, t, p)
names(out) <- c("B - C", "SE", "T", "P")
round(out, 3)

## Interactions
hunger <- read.csv("hunger.csv")
hunger <- hunger[hunger$Sex != "Both sexes", ]
head(hunger)
lm1 <- lm(hunger$Numeric ~ hunger$Year)
plot(hunger$Year, hunger$Numeric, pch = 19, col = "blue")
# add the line model
lines(hunger$Year, lm1$fitted, lwd = 3, col = "darkgrey")
# color by male/female
plot(hunger$Year, hunger$Numeric, pch = 19)
points(hunger$Year, hunger$Numeric, pch = 19, col = (hunger$Sex == "Male") * 1 + 1)

lmM <- lm(hunger$Numeric[hunger$Sex == "Male"] ~ hunger$Year[hunger$Sex == "Male"])
lmF <- lm(hunger$Numeric[hunger$Sex == "Female"] ~ hunger$Year[hunger$Sex == "Female"])
plot(hunger$Year, hunger$Numeric, pch = 19)
points(hunger$Year, hunger$Numeric, pch = 19, col = ((hunger$Sex == "Male") * 1 + 1))
lines(hunger$Year[hunger$Sex == "Male"], lmM$fitted, col = "black", lwd = 3)
lines(hunger$Year[hunger$Sex == "Female"], lmF$fitted, col = "red", lwd = 3)

# Two lines, same slope
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex)
plot(hunger$Year, hunger$Numeric, pch = 19)
points(hunger$Year, hunger$Numeric, pch = 19, col = ((hunger$Sex == "Male") * 1 + 1))
abline(c(lmBoth$coeff[1], lmBoth$coeff[2]), col = "red", lwd = 3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3]), lmBoth$coeff[2], col = "black", lwd = 3)

# Two lines, different slopes (interactions)
lmBoth <- lm(hunger$Numeric ~ hunger$Year + hunger$Sex + hunger$Sex * hunger$Year)
plot(hunger$Year, hunger$Numeric, pch = 19)
points(hunger$Year, hunger$Numeric, pch = 19, col = ((hunger$Sex == "Male") * 1 + 1))
abline(c(lmBoth$coeff[1], lmBoth$coeff[2]), col = "red", lwd = 3)
abline(c(lmBoth$coeff[1] + lmBoth$coeff[3], lmBoth$coeff[2] + lmBoth$coeff[4]), col = "black", lwd = 3)
summary(lmBoth)

## Multivariable Regression Examples
# Simulation 1
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 2
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), 1.5 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 3
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), .9 + runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 4
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(.5 + runif(n/2), runif(n/2)); 
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 5
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2, -1, 1), runif(n/2, -1, 1)); 
beta0 <- 0; beta1 <- 2; tau <- 0; tau1 <- -4; sigma <- .2
y <- beta0 + x * beta1 + t * tau + t * x * tau1 + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t + I(x * t))
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2] + coef(fit)[4], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

# Simulation 6
p <- 1
n <- 100; x2 <- runif(n); x1 <- p * runif(n) - (1 - p) * x2 
beta0 <- 0; beta1 <- 1; tau <- 4 ; sigma <- .01
y <- beta0 + x1 * beta1 + tau * x2 + rnorm(n, sd = sigma)
plot(x1, y, type = "n", frame = FALSE)
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

if (require(rgl) == FALSE) install.packages("rgl")
library(rgl)
plot3d(x1, x2, y)

plot(resid(lm(x1 ~ x2)), resid(lm(y ~ x2)), frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(x1 ~ x2))) ~ I(resid(lm(y ~ x2)))), lwd = 2)

## Residuals, Diagnostics, Variation
data(swiss); par(mfrow = c(2,2))
fit <- lm(Fertility ~ ., data = swiss); plot(fit)

# Influential, leverage, outlier
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

# Hat influence
n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y , frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))
fit <- lm(y ~ x)
round(dfbetas(fit)[1:10, 2], 3)
round(hatvalues(fit)[1:10], 3)

x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2)            
round(dfbetas(fit2)[1 : 10, 2], 3)
round(hatvalues(fit2)[1 : 10], 3)

dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
summary(lm(V1 ~ . -1, data = dat))$coef
fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')

data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

## Multiple Variables
#Variance inflation
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
        y <- x1 + rnorm(n, sd = .3)
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

y <- x1 + rnorm(n, sd = .3)
a <- summary(lm(y ~ x1))$cov.unscaled[2,2]
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2,2],
  summary(lm(y~ x1 + x2 + x3))$cov.unscaled[2,2]) / a
temp <- apply(betas, 1, var); temp[2 : 3] / temp[1]

# Model Comparison and Search
data(swiss);
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit, Fertility ~ Agriculture + Examination)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
  summary(fit3)$cov.unscaled[2,2]) / a 

if(require(car) == FALSE) install.packages("car")
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)
sqrt(vif(fit)) #I prefer sd 

## Binary Data GLMs
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              , destfile="./ravensData.rda",method="curl")
load("./ravensData.rda")
head(ravensData)

lmRavens <- lm(ravensData$ravenWinNum ~ ravensData$ravenScore)
summary(lmRavens)$coef

## GLM and Odds
library(manipulate)
x <- seq(-10, 10, length = 1000)
manipulate(
        plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)), 
             type = "l", lwd = 3, frame = FALSE),
        beta1 = slider(-2, 2, step = .1, initial = 2),
        beta0 = slider(-2, 2, step = .1, initial = 0)
)

# Ravens logistic regression
logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)
plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",xlab="Score",ylab="Prob Ravens Win")

# Odds ratios and confidence intervals
exp(logRegRavens$coeff)
exp(confint(logRegRavens))

# ANOVA for logistic regression
anova(logRegRavens,test="Chisq")

## Possion Regression
par(mfrow = c(1, 3))
plot(0:10, dpois(0:10, lambda = 2), type = "h", frame = FALSE)
plot(0:20, dpois(0:20, lambda = 2), type = "h", frame = FALSE)
plot(0:100, dpois(0:100, lambda = 2), type = "h", frame = FALSE)

x <- 0 : 10000; lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu)^2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

download.file("https://dl.dropboxusercontent.com/u/7710864/data/gaData.rda",destfile="./gaData.rda",method="curl")
load("./gaData.rda")
gaData$julian <- julian(gaData$date)
head(gaData)

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits ~ gaData$julian)
abline(lm1, col = "red", lwd = 3)

round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)

plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
glm1 <- glm(gaData$visits ~ gaData$julian,family="poisson")
abline(lm1,col="red",lwd=3); lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")

# Model agnostic standard erros
if(require(sandwich) == FALSE) install.packages("sandwich")
library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
        cf <- coef(object); pnames <- names(cf)
        if (missing(parm))
                parm <- pnames
        else if (is.numeric(parm))
                parm <- pnames[parm]
        a <- (1 - level)/2; a <- c(a, 1 - a)
        pct <- stats:::format.perc(a, 3)
        fac <- qnorm(a)
        ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                                   pct))
        ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
        ci[] <- cf[parm] + ses %o% fac
        ci
}
confint(glm1)
confint.agnostic(glm1)

glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted Counts")
points(julian(gaData$date),glm1$fitted,col="red",pch=19)

glm2 <- glm(gaData$simplystats ~ julian(gaData$date),offset=log(visits+1),
            family="poisson",data=gaData)
plot(julian(gaData$date),gaData$simplystats/(gaData$visits+1),col="grey",xlab="Date",
     ylab="Fitted Rates",pch=19)
lines(julian(gaData$date),glm2$fitted/(gaData$visits+1),col="blue",lwd=3)

## Hodgepodge
n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
knots <- seq(0, 8 * pi, length = 20); 
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot)^2)
xMat <- cbind(1, x, x^2, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
t <- seq(0, 2, by = .001); n <- length(t)
c4 <- sin(2 * pi * notes4[1] * t); e4 <- sin(2 * pi * notes4[3] * t); 
g4 <- sin(2 * pi * notes4[5] * t)
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
fit <- lm(chord ~ x - 1)
plot(c(0, 9), c(0, 1.5), xlab = "Note", ylab = "Coef^2", axes = FALSE, frame = TRUE, type = "n")
axis(2)
axis(1, at = 1 : 8, labels = c("c4", "d4", "e4", "f4", "g4", "a4", "b4", "c5"))
for (i in 1 : 8) abline(v = i, lwd = 3, col = grey(.8))
lines(c(0, 1 : 8, 9), c(0, coef(fit)^2, 0), type = "l", lwd = 3, col = "red")
a <- fft(chord); plot(Re(a)^2, type = "l")

## Logit Regression from "http://www.ats.ucla.edu/stat/r/dae/logit.htm"
if(require(aod) == FALSE) install.packages("aod")
library(aod)
if(require(ggplot2) == FALSE) install.packages("ggplot2")
library(ggplot2)

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
summary(mydata)
sapply(mydata, sd)
xtabs(~admit + rank, data = mydata)

mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)
confint(mylogit)
confint.default(mylogit)

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
l <- cbind(0, 0 , 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

exp(coef(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))

newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))
newdata1

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), 
                                    gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link", se = TRUE))
newdata3 <- within(newdata3, {
        PredictedProb <- plogis(fit)
        LL <- plogis(fit - (1.96 * se.fit))
        UL <- plogis(fit + (1.96 * se.fit))
})
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
        ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
        size = 1)
with(mylogit, null.deviance - deviance)
with(mylogit, df.null - df.residual)
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
logLik(mylogit)
