#q1
data("mtcars")
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)$coef[3,1]

#q2
fitwt <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
fit <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fitwt)
summary(fit)

#q3
fit2 <- lm(mpg ~ factor(cyl) + wt + factor(cyl) * wt, data = mtcars)
summary(fit2)
anova(fitwt, fit2)      # the p-value is 0.1239, failed to reject null hypothesis(coef is 0)

#q4
summary(lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars))

#q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
hatvalues(lm(y ~ x))    # the 5th

#q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
dfbetas(lm(y ~ x))      # the 5th point has highest hat value