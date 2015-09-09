setwd("./Chapter6.Statistical_Inference/")
## Chapter 6 Statistical Inference
x <- c(-0.5, 0, 1, 1, 1.5)
y <- c(0, 0, 2, 0, 0)
plot(x, y, lwd = 3, frame = FALSE, type = "l")
# x is 0.5
pbeta(0.5, 2, 1)
# p is 0.5
qbeta(0.5, 2, 1)

## Expectations
if(require(UsingR) == F) install.packages(("UsingR"))
library(UsingR)
data(galton)
library(manipulate)
myHist <- function(mu){
        hist(galton$child,col="blue",breaks=100)
        lines(c(mu, mu), c(0, 150),col="red",lwd=5)
        mse <- mean((galton$child - mu)^2)
        text(63, 150, paste("mu = ", mu))
        text(63, 140, paste("Imbalance = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

hist(galton$child, col = "blue", breaks = 100)
meanChild <- mean(galton$child)
lines(rep(meanChild, 100), seq(0, 150, length = 100), col = "red", lwd = 5)
# a vertical line will be drawn


## Common Distributions
n <- 5
pvals <- seq(0, 1, length = 1000)
plot(c(0, 1), c(0, 1.2), type = "n", frame = FALSE, xlab = "p", ylab = "likelihood")
text((0:n)/n, 1.1, as.character(0:n))
sapply(0:n, function(x) {
        phat <- x/n
        if(x == 0) lines(pvals, ((1 - pvals)/(1 - phat))^(n - x), lwd = 3)
        else if(x == n) lines(pvals, (pvals/phat)^x, lwd = 3)
        else lines(pvals, (pvals/phat)^x * ((1 - pvals)/(1 - phat))^(n - x), lwd = 3)
})
title(paste("Likelihoods for n = ", n))

plot(pvals, dbinom(7, 8, pvals) / dbinom(7, 8, 7/8), lwd = 3, frame = FALSE, type = "l",
     xlab = "p", ylab = "likelihood")

zvals <- seq(-3, 3, length = 1000)
plot(zvals, dnorm(zvals), type = "l", lwd = 3, frame = FALSE, xlab = "z", ylab = "Density")
sapply(-3:3, function(k) abline(v = k))
# Multiple vertical lines plotted

ppois(3, lambda = 2.5 * 4)
pbinom(2, size = 500, prob = .01)
ppois(2, lambda = 500 * .01)


## Standard error of the mean
nosim <- 1000
n <- 10
sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))
1/sqrt(n)

# Standard uniforms have variance 1/12
sd(apply(matrix(runif(nosim * n), nosim), 1, mean))
1/sqrt(12 * n)

# Poisson(4) have variance 4
sd(apply(matrix(rpois(nosim * n, 4), nosim), 1, mean))
2/sqrt(n)

# Fair coin flips have variance 0.25
sd(apply(matrix(sample(0:1, nosim * n, replace = TRUE), nosim), 1, mean))
1/(2 * sqrt((n)))


## Binomial distribution
choose(8, 7) * .5 ^ 8 + choose(8, 8) * .5 ^ 8
pbinom(6, size = 8, prob = .5, lower.tail = FALSE)
round(1/sqrt(10^(1:6)), 3)


## Normal distribution
pnorm(1160, mean = 1020, sd = 50, lower.tail = FALSE)
pnorm((1160 - 1020) / 50, lower.tail = FALSE)

qnorm(0.75, mean = 1020, sd = 50)


## Poisson
ppois(3, lambda = 2.5 * 4)
pbinom(2, size = 500, prob = 0.01)
ppois(2, lambda = 500 * 0.01)


## Asympototics and LLN
n <- 1000
means <- cumsum(rnorm(n))/(1:n)
means <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)

## Confidence interval
library(UsingR)
data("father.son")
x <- father.son$sheight
(mean(x) + c(1, -1) * qnorm(0.975) * sd(x)/sqrt(length(x)))/12  # in feet

# 56 of 100 votes
0.56 + c(-1, 1) * qnorm(0.975) * sqrt(0.56 * 0.44/100)
binom.test(56, 100)$conf.int

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- rbinom(nosim, prob = p, size = n)/n
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})
plot(pvals, coverage, type = "l")
abline(h = 0.95)

n <- 100
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage2 <- sapply(pvals, function(p) {
        phats <- rbinom(nosim, prob = p, size = n)/n
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})
plot(pvals, coverage2, type = "l")
abline(h = 0.95)

# Adding 2 successes and failures
n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
        phats <- (rbinom(nosim, prob = p, size = n) + 2)/(n + 4)
        ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
        mean(ll < p & ul > p)
})
plot(pvals, coverage, type = "l")
abline(h = 0.95)

x <- 5
t <- 94.32
lambda <- x/t
round(lambda + c(-1, 1) * qnorm(0.975) * sqrt(lambda/t), 3)
poisson.test(x, T = 94.32)$conf

# Simulating the poisson coverage
lambdavals <- seq(0.005, 0.1, by = 0.01)
nosim <- 1000
t <- 100
coverage <- sapply(lambdavals, function(lambda) {
        lhats <- rpois(nosim, lambda = lambda * t)/t
        ll <- lhats - qnorm(0.975) * sqrt(lhats/t)
        ul <- lhats + qnorm(0.975) * sqrt(lhats/t)
        mean(ll < lambda & ul > lambda)
})
plot(lambdavals, coverage, type = "l")
abline(h = 0.95)


## T confidence intervals example
data(sleep)
head(sleep)
g1 <- sleep$extra[1:10]; g2 <- sleep$extra[11:20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10

mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paird = TRUE, data = sleep)


## Independent group T intervals
sp <- sqrt(((8 - 1) * 15.34^2 + (21 - 1) * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, (8 + 21 - 2)) * sp * (1/8 + 1/21)^.5

x1 <- sleep$extra[sleep$group == 1]
x2 <- sleep$extra[sleep$group == 2]
n1 <- length(g1); n2 <- length(g2)                              
sp <- sqrt(((n1 - 1) * sd(x1)^2 + (n2 - 1) * sd(x2)^2) / (n1 + n2 - 2))
# mean difference
md <- mean(g2) - mean(g1)
# standard error of mean difference
semd <- sp * sqrt(1/n1 + 1/n2)
rbind(
        md + c(-1, 1) * qt(.975, n1 + n2 - 2) *semd,
        t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
        t.test(g2, g1, paired = TRUE)$conf
)

library(datasets); data("ChickWeight"); library(reshape2)
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1:2)] <- paste("time", names(wideCW)[-(1:2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW, gain = time21 - time0)

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
        t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
        t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)


## T tests
qt(.95, 15)
qt(.975, 15)

library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)

wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE, var.equal= TRUE, data = wideCW14)


## P-values
pt(2.5, 15, lower.tail = FALSE)

ppois(9, 5, lower.tail = FALSE)

## Homework 3
library(datasets)
data("mtcars")
round(t.test(mtcars$mpg)$conf.int)

# sd is 1 for 9 pairs differences, what's the mean of difference if lower point of 95% t confidence is 0
round(qt(.975, df = 9 - 1) * 1/sqrt(9), 2)

# 95% T interval of MPG comparing 4 and 6 cylindar cars
m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
t.test(m4, m6, var.equal = TRUE)$conf.int
# or below
mtcars46 <- subset(mtcars, cyl %in% c(4, 6))
t.test(mpg ~ cyl, paired = FALSE, var.equal = TRUE, data = mtcars46)

n1 <- n2 <- 9
x1 <- -3  ##treated
x2 <- 1  ##placebo
s1 <- 1.5  ##treated
s2 <- 1.8  ##placebo
spsq <- ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)

## Quiz 3
#q1
qt(.975, df = 9 -1) * 30/sqrt(9)
#q2
-2 + c(-1,1) * qt(.975, df = 9 - 1) * 2.6/sqrt(9)
#q4
newSp <- sqrt(((10 - 1) * .6 + (10 - 1) * .68) / (10 + 10 -2))
(3 - 5) + c(-1, 1) * qt(.975, 10 + 10 - 2) * newSp * sqrt(1/10 + 1/10)
#q6
newSp6 <- sqrt(((100 - 1) * .5^2 + (100 - 1) * 2^2) / (100 + 100 - 2))
(6 - 4) + c(-1, 1) * qnorm(0.975) * newSp6 * sqrt(1/100 + 1/100)
#q7
newSp7 <- sqrt(((9 - 1) * 1.5^2 + (9 - 1) * 1.8^2) / (9 + 9 - 2))
(-3 - 1) + c(-1, 1) * qt(.95, 9 + 9 - 2) * newSp7 * sqrt(1/9 + 1/9)


## Power
mu0 = 30
mua = 32
sigma = 4
n = 16
z <- qnorm(1 - alpha)
pnorm(mu0 + z * sigma/sqrt(n), mean = mu0, sd = sigma/sqrt(n), lower.tail = FALSE)


## Calculating Power
library(manipulate)
mu0 = 30
myplot <- function(sigma, mua, n, alpha) {
        g = ggplot(data.frame(mu = c(27, 36)), aes = (x = mu))
        g = g + stat_function(fun = dnorm, geom = "line",
                              args = list(mean = mu0, sd = sigma/sqrt(n)),
                              size = 2, col = "red")
        g = g + stat_function(fun = dnorm, geom = "line",
                              args = list(mean = mua, sd = sigma/sqrt(n)),
                              size = 2, col = "blue")
        xitc = mu0 + qnorm(1 - alpha) * sigma/sqrt(n)
        g = g + geom_vline(xintercept = xitc, size = 3)
        g
}
manipulate(
        myplot(sigma, mua, n, alpha),
        sigma = slider(1, 10, step = 1, initial = 4),
        mua = slider(30, 35, step = 1, initial = 32),
        n = slider(1, 50, step = 1, initial = 16),
        alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)

## T test power
power.t.test(n = 16, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$power
power.t.test(n = 16, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$power
power.t.test(n = 16, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$power
# the above 3 cases give same result, because delta/sd is the same, it's the effect size

power.t.test(power = 0.8, delta = 2/4, sd = 1, type = "one.sample", alt = "one.sided")$n
power.t.test(power = 0.8, delta = 2, sd = 4, type = "one.sample", alt = "one.sided")$n
power.t.test(power = 0.8, delta = 100, sd = 200, type = "one.sample", alt = "one.sided")$n


## Multiple testing
set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000) {
        y <- rnorm(20)
        x <- rnorm(20)
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
sum(pValues < 0.05)

sum(p.adjust(pValues, method = "bonferroni") < 0.05)
sum(p.adjust(pValues, method = "BH") < 0.05)

set.seed(1010093)
pValues <- rep(NA, 1000)
for(i in 1:1000) {
        y <- rnorm(20)
        # First 500 beta = 0, last 500 beta = 2
        if(i <= 500) {y <- rnorm(20)} else{ y <- rnorm(20, mean = 2*x)}
        pValues[i] <- summary(lm(y ~ x))$coeff[2,4]
}
trueStatus <- rep(c("zero", "not zero"), each = 500)
table(pValues < 0.05, trueStatus)

table(p.adjust(pValues, method = "bonferroni") < 0.05, trueStatus)
table(p.adjust(pValues, method = "BH") < 0.05, trueStatus)

par(mfrow = c(1,2))
plot(pValues, p.adjust(pValues, method = "bonferroni"), pch = 19)
plot(pValues, p.adjust(pValues, method = "BH"), pch = 19)


## Bootstrapping
library(UsingR)
data("father.son")
x <- father.son$sheight
n <- length(x)
B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
resampledMedians <- apply(resamples, 1, median)

B <- 10000
resamples <- matrix(sample(x, n * B, replace = TRUE), B, n)
medians <- apply(resamples, 1, median)
sd(medians)
quantile(medians, c(0.025, 0.975))

g = ggplot(data.frame(medians = medians), aes(x = medians))
g = g + geom_histogram(color = "black", fill = "lightblue", binwidth = 0.05)
g


## Permutation test
subdata <- InsectSprays[InsectSprays$spray %in% c("B", "C"),]
y <- subdata$count
group <- as.character(subdata$spray)
testStat <- function(w, g) mean(w[g == "B"]) - mean(w[g == "C"])
observedStat <- testStat(y, group)
permutations <- sapply(1:10000, function(i) testStat(y, sample(group)))
observedStat
mean(permutations > observedStat)


## Homework 4
data(mtcars)
mn <- mean(mtcars$mpg)
s <- sd(mtcars$mpg)
z <- qnorm(.05)
mu0 <- mn - z * s / sqrt(nrow(mtcars))

m4 <- mtcars$mpg[mtcars$cyl == 4]
m6 <- mtcars$mpg[mtcars$cyl == 6]
p <- t.test(m4, m6, paired = FALSE, alternative="two.sided", var.equal=FALSE)$p.value

# 55 out of 100 flips
ans <- round(pbinom(54, prob = .5, size = 100, lower.tail = FALSE),4)

# 520 hits per day, 30 days, actually received 15,800 hits
pv <- ppois(15800 - 1, lambda = 520 * 30, lower.tail = FALSE)
pnorm(15800 / 30, mean = 520, sd = sqrt(520 / 30), lower.tail = FALSE)

# Suppose that in an AB test, one advertising scheme led to an average 
# of 10 purchases per day for a sample of 100 days, while the other led 
# to 11 purchaces per day, also for a sample of 100 days. Assuming a 
# common standard deviation of 4 purchases per day. Assuming that the 
# groups are independent and that they days are iid, 
# perform a Z test of equivalence.
m1 <- 10; m2 <- 11
n1 <- n2 <- 100
s <- 4
se <- s * sqrt(1 / n1 + 1 / n2)
ts <- (m2 - m1) / se
pv <- 2 * pnorm(-abs(ts))

power <- pnorm(10 + qnorm(.95) * .4, mean = 11, sd = .4, lower.tail = FALSE)

n <- (qnorm(.95) + qnorm(.8)) ^ 2 * .04 ^ 2 / .01^2

mpg8 <- mtcars$mpg[mtcars$cyl == 8]
mpg6 <- mtcars$mpg[mtcars$cyl == 6]
m8 <- mean(mpg8); m6 <- mean(mpg6)
s8 <- sd(mpg8); s6 <- sd(mpg6)
n8 <- length(mpg8); n6 <- length(mpg6)
p <- t.test(mpg8, mpg6, paired = FALSE, alternative="two.sided", var.equal=TRUE)$p.value
mixprob <- (n8 - 1) / (n8 + n6 - 2)
s <- sqrt(mixprob * s8 ^ 2  +  (1 - mixprob) * s6 ^ 2)
z <- (m8 - m6) / (s * sqrt(1 / n8 + 1 / n6))
pz <- 2 * pnorm(-abs(z))
## Hand calculating the T just to check
#2 * pt(-abs(z), df = n8 + n6 - 2)

## Quiz 4
#q1
base <- c(140, 138, 150, 148, 135)
w2 <- c(132, 135, 151, 146, 130)
t.test(base, w2, paired = TRUE)

#q2
1100 + c(-1, 1) * qt(.975, 9-1) * sqrt(9)

#q3
pbinom(2, prob = .5, size = 4, lower.tail = FALSE)

#q5
sp <- sqrt(((9-1)*1.5^2 + (9-1)*1.8^2)/(9+9-2))
se <- sp * sqrt(1/9 + 1/9)
t <- (-3 - 1) + c(-1,1)*qt(.975, 16) * se

#q4
ppois(10, 17.87, lower.tail = FALSE)

#q7
power.t.test(n = 100, delta = 0.01, sd = 0.04, type = "one.sample", alt = "one.sided")

#q8
(qnorm(.95) + qnorm(.9)) ^ 2 * .04 ^ 2 / .01^2