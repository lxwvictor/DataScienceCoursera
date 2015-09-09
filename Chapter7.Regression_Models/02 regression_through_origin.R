library(ggplot2)
source("load_data.R")

# Formula for a straight line
m = 0.5
c = 1
ggplot(data.frame(x=0, y=0), aes(x=x, y=y)) +
    xlim(-2, 2) + ylim(-2, 2) +
    geom_point() +
    geom_abline(intercept=c, slope=m) +
    geom_segment(x=0, xend=0, y=0, yend=c, linetype=2, color="red") +
    geom_text(label="Intercept c = 1", x=0.1, y=c/2, color="red", size=10, hjust=0) +
    geom_segment(x=0, xend=2, y=c, yend=c, linetype=2, color="blue") +
    geom_segment(x=2, xend=2, y=c, yend=(m * 2 + c), linetype=2, color="blue") +
    geom_text(label="Slope m = 0.5", x=1, y=(m + c + 0.1), color="blue", size=10,
              angle=atan(m)/pi*180, vjust=-1)

# Scatterplot of income vs sqft
p <- ggplot(households, aes(x=sqft, y=income)) +
    geom_point() +
    xlim(0, NA) + ylim(0, NA)
p

# Which is the best line?
line.colors = c("red", "orange", "blue", "green", "violet")
lines <- data.frame(intercept=0, slope=1:5, color=line.colors)
p + geom_abline(aes(intercept=intercept, slope=slope, color=color), data=lines)

# Measuring errors
m = 3
errors = data.frame(
    sqft=households$sqft,
    max=households$income,
    min=m * households$sqft)
p +
    geom_abline(slope=m, color="blue") +
    geom_linerange(aes(x=sqft, y=max, ymax=max, ymin=min), data=errors, color="red")

# Using lm() to find the optimal slope
best.slope <- lm(income ~ 0 + sqft, households)
m <- coef(best.slope)["sqft"]
p +
    geom_abline(slope=m, color="blue") +
    geom_text(label=paste0("y = ", prettyNum(m), "x"), x=0, y=0, color="blue", size=8, hjust=0)
    
