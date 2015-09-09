library(ggplot2)
source("load_data.R")

# The intercept
households.shifted <- households
households.shifted$sqft <- scale(households.shifted$sqft, scale=FALSE)
means = data.frame(sqft=mean(households.shifted$sqft), income=mean(households.shifted$income))
model <- lm(income ~ sqft, households.shifted)
c <- coef(model)["(Intercept)"]
m <- coef(model)["sqft"]
ggplot(households.shifted, aes(x=sqft, y=income)) +
    geom_point() +
    ylim(0, NA) +
    geom_point(x=means$sqft, y=means$income, color="red", size=5) +
    geom_abline(intercept=c, slope=m, color="blue") +
    geom_text(label=paste0("y = ", prettyNum(c), " + ", prettyNum(m), "x"),
              x=0, y=0, color="blue", size=8, hjust=0.5)

# Normalisation
households.normalised <- as.data.frame(scale(households))
means = data.frame(sqft=mean(households.normalised$sqft), income=mean(households.normalised$income))
model <- lm(income ~ sqft, households.normalised)
c <- coef(model)["(Intercept)"]
m <- coef(model)["sqft"]
ggplot(households.normalised, aes(x=sqft, y=income)) +
    geom_point() +
    geom_point(x=means$sqft, y=means$income, color="red", size=5) +
    geom_abline(intercept=c, slope=m, color="blue") +
    geom_text(label=paste0("y = ", prettyNum(c), " + ", prettyNum(m), "x"),
              x=0, y=min(households.normalised$income), color="blue", size=8, hjust=0.5)
cor(households$income, households$sqft)

# Switching the Predictor and Outcome
means = data.frame(sqft=mean(households$sqft), income=mean(households$income))
model <- lm(sqft ~ income, households)
c <- coef(model)["(Intercept)"]
m <- coef(model)["income"]
ggplot(households, aes(x=income, y=sqft)) +
    geom_point() +
    xlim(0, NA) + ylim(0, NA) +
    geom_point(x=means$income, y=means$sqft, color="red", size=5) +
    geom_abline(intercept=c, slope=m, color="blue") +
    geom_text(label=paste0("y = ", prettyNum(c), " + ", prettyNum(m), "x"),
              x=0, y=0, color="blue", size=8, hjust=0)
m * var(households$income) / var(households$sqft)
