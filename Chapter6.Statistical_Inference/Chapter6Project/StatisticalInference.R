mns = NULL
for (i in 1:1000) mns = c(mns, mean(runif(40)))
hist(mns)


set.seed(1)
lambda = 0.2
nosim <- 1000
n <- 40
exp =  NULL
for (i in 1:1000) exp = rbind(exp, rexp(n, lambda))
expMean <- apply(exp, 1, mean)
hist(expMean, main = "Average of simulations", xlab = "Average", breaks = 30)
abline(v = mean(expMean), col = 3, lwd = 5)
abline(v = 1/lambda, col = 2, lwd = 3)

dst <- density(expMean)
plot(dst$x, dst$y, type = "n", main = "The distribution of sample mean", xlab = "Mean", ylab = "Density")
lines(seq(3, 7, length = 100), dnorm(seq(3, 7, length = 100), mean = 5, sd = (1/lambda)/sqrt(n)), col = grey(.8), lwd = 3)
lines(dst$x, dst$y, lwd = 2)

data("ToothGrowth")
library(lattice)
library(ggplot2)
tooth <- transform(ToothGrowth, dose = factor(dose))
plot1 <- xyplot(len ~ dose | supp, data = tooth, layout = c(2,1))
plot2 <- xyplot(len ~ supp | dose, data = tooth, layout = c(3,1))
print(plot1, position = c(0,0,0.5,1), more = TRUE)
print(plot2, position = c(0.5,0,1,1))

gVC1 <- tooth[which(tooth$supp == "VC" & tooth$dose == 0.5), 1]
gVC2 <- tooth[which(tooth$supp == "VC" & tooth$dose == 1), 1]
gVC3 <- tooth[which(tooth$supp == "VC" & tooth$dose == 2), 1]
gOJ1 <- tooth[which(tooth$supp == "OJ" & tooth$dose == 0.5), 1]
gOJ2 <- tooth[which(tooth$supp == "OJ" & tooth$dose == 1), 1]
gOJ3 <- tooth[which(tooth$supp == "OJ" & tooth$dose == 2), 1]

t.test(gVC1, gOJ1, paired = TRUE)$conf
t.test(gVC2, gOJ2, paired = TRUE)$conf
t.test(gVC3, gOJ3, paired = TRUE)$conf

t.test(gVC1, gVC2, paired = TRUE)$conf
t.test(gVC2, gVC3, paired = TRUE)$conf
t.test(gOJ1, gOJ2, paired = TRUE)$conf
t.test(gOJ2, gOJ3, paired = TRUE)$conf
