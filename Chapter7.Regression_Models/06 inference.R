library(ggplot2)
source("load_data.R")

# t values for linear models
model <- lm(income ~ sqft, households)
summary(model)

# Confidence interval for coefficients
confint(model, level=0.95)
confint(model, level=0.80)

# Visualising the confidence interval for the regression line
confidence <- 0.95
c <- coef(best.line)["(Intercept)"]
m <- coef(best.line)["sqft"]
newdata <- data.frame(sqft=seq(min(households$sqft), max(households$sqft), length.out=200))
ci.line <- data.frame(predict(model, newdata, interval="confidence", level=confidence))
ci.line$sqft <- newdata$sqft
p <- ggplot(households, aes(x=sqft, y=income)) +
    geom_point() +
    geom_point(x=means$sqft, y=means$income, color="red", size=5) +
    geom_abline(intercept=c, slope=m, color="blue")
p + geom_ribbon(aes(x=sqft, y=fit, ymin = lwr, ymax = upr), ci.line, fill="blue", alpha=0.2)

# Confidence interval for predictions
ci.pred <- data.frame(predict(model, newdata, interval="prediction", level=confidence))
ci.pred$sqft <- newdata$sqft
p + geom_ribbon(aes(x=sqft, y=fit, ymin = lwr, ymax = upr), ci.pred, fill="yellow", alpha=0.2) +
    geom_ribbon(aes(x=sqft, y=fit, ymin = lwr, ymax = upr), ci.line, fill="blue", alpha=0.2)
