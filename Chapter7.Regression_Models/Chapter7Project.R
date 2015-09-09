library(datasets); data(swiss); require(stats); require(graphics)
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))

data(swiss); par(mfrow = c(2,2))
fit <- lm(Fertility ~ ., data = swiss); plot(fit)

plot(mtcars$mpg, col = mtcars$am + 2)
abline(h = tapply(mtcars$mpg, factor(mtcars$am), mean), col = c(2, 3))
legend("topright", pch = 1, col = c(2, 3), legend = c("Auto", "Manual"))

boxplot(mpg ~ am, col = c(2, 3), xlab = "Transmission (0 = auto, 1 = manual)", ylab = "MPG (miles/gallon", data = mtcars)

autoMPG <- mtcars[mtcars$am == 0, ]$mpg
manualMPG <- mtcars[mtcars$am == 1,]$mpg
t.test(autoMPG, manualMPG, paired = FALSE)

t.test(mpg~am, data = mtcars, paird = FALSE)

full.model <- lm(mpg ~ ., data = mtcars)
summary(full.model)
reduced.model <- step(full.model, direction = "backward")
summary(reduced.model)
anova(reduced.model, full.model)
plot(resid(reduced.model), col = 2)
abline(h = 0, col = 2)

par(mfrow = c(2,2))
plot(reduced.model)
