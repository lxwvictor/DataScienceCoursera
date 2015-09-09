# q1, q2
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
summary(lm(y ~ x))

# q3
data(mtcars)
mwfit <- lm(mpg ~ wt, mtcars)
meanWt <- mean(mtcars$wt)
predict(mwfit, newdata = data.frame(wt = meanWt), interval = "confidence", level = .95)

# q4
mwfit$coef

# q5
predict(mwfit, newdata = data.frame(wt = 3), interval = "prediction", level = .95)

# q6
sumCoef <- summary(mwfit)$coefficients
(sumCoef[2,1] + c(-1, 1) * qt(.975, df = mwfit$df) * sumCoef[2, 2]) * 2

# q9
mwfit <- lm(mpg ~ wt, mtcars)
sum(resid(mwfit) ^ 2) / sum((mtcars$mpg - mean(mtcars$mpg)) ^ 2)