#q1
library(MASS)
data(shuttle)
head(shuttle)
summary(shuttle)
str(shuttle)
mdl <- glm(use ~ wind, family = "binomial", data = shuttle)
summary(mdl)
confint(mdl)
exp(cbind(OR = coef(mdl), confint(mdl)))

#q2
mdl2 <- glm(use ~ wind + magn, family = "binomial", data = shuttle)
summary(mdl2)
coef(mdl2)
exp(cbind(OR = coef(mdl2), confint(mdl2)))

#q3
mdl3 <- glm((1 - I(1 * (use == 'noauto'))) ~ wind, family = "binomial", data = shuttle)
summary(mdl3)
exp(coef(summary(mdl3)))

#q4
data("InsectSprays")
str(InsectSprays)
head(InsectSprays)
mdl4 <- glm(count ~ spray, family = "poisson", data = InsectSprays)
summary(mdl4)

#q6 just plot x and y
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots <- 0
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot) ^2)
xMat <- cbind(0, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)