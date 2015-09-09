library(ggplot2)
source("load_data.R")

# Scatterplot of income vs sqft
p <- ggplot(households, aes(x=sqft, y=income)) +
    geom_point() +
    xlim(0, NA) + ylim(0, NA)

# Which is the best line with intercept?
line.colors = c("red", "orange", "blue", "green", "violet")
lines <- data.frame(intercept=seq(-2000, 2000, 1000), slope=3, color=line.colors)
p + geom_abline(aes(intercept=intercept, slope=slope, color=color), data=lines)

# It's not that simple...
p +
    geom_abline(intercept=-1000, slope=3, color="red") +
    geom_abline(intercept=3000, slope=1, color="orange") +
    geom_abline(intercept=1000, slope=2, color="blue") +
    geom_abline(intercept=-1000, slope=4, color="green") +
    geom_abline(intercept=3000, slope=0.5, color="violet")

# Best fit line passes through the means
means = data.frame(sqft=mean(households$sqft), income=mean(households$income))
lines <- data.frame(intercept=means$income - 1:5 * means$sqft, slope=1:5, color=line.colors)
p + geom_point(x=means$sqft, y=means$income, color="red", size=5) +
    geom_abline(aes(intercept=intercept, slope=slope, color=color), data=lines)
    
# Using lm() to find the best line
best.line <- lm(income ~ sqft, households)
c <- coef(best.line)["(Intercept)"]
m <- coef(best.line)["sqft"]
p + geom_point(x=means$sqft, y=means$income, color="red", size=5) +
    geom_abline(intercept=c, slope=m, color="blue") +
    geom_text(label=paste0("y = ", prettyNum(c), " + ", prettyNum(m), "x"),
              x=0, y=0, color="blue", size=8, hjust=0)
