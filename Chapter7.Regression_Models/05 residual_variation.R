library(ggplot2)
library(grid)
source("load_data.R")

# Residual plots are useful for examining model fit
model <- lm(income ~ sqft, households)
e <- data.frame(e=residuals(model), sqft=households$sqft)
ggplot(e, aes(x=sqft, xend=sqft, y=e, yend=0)) +
    geom_point() +
    xlim(0, NA) +
    geom_hline(y=0, color="blue") +
    geom_segment(color="red")

# Example of poor model fit
data <- data.frame(x=1:100, y=1:100 + rnorm(100, sd=5) + 10*sin((1:100) / 15))
model <- lm(y ~ x, data)
model.plot <- ggplot(data, aes(x=x, y=y)) +
    geom_point() +
    geom_abline(intercept=coef(model)[1], slope=coef(model)[2], color="blue")
resid.plot <- ggplot(data.frame(x=data$x, e=residuals(model)),
                     aes(x=x, xend=x, y=e, yend=0)) +
    geom_point() +
    geom_hline(y=0, color="blue") +
    geom_segment(color="red")
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 2)))
print(model.plot, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(resid.plot, vp=viewport(layout.pos.row=1, layout.pos.col=2))
