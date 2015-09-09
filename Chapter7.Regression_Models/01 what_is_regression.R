source("load_data.R")

# Examine each variable
library(ggplot2)
library(grid)
hist.w.mean <- function(data, variable) {
    ggplot(data, aes_string(x=variable)) +
        geom_histogram() +
        geom_vline(xintercept=mean(data[[variable]], na.rm=TRUE),
                   color="red", linetype="dashed", size=1)
}
grid.newpage()
pushViewport(viewport(layout=grid.layout(1, 3)))
print(hist.w.mean(households, "people"), vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(hist.w.mean(households, "sqft"), vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(hist.w.mean(households, "income"), vp=viewport(layout.pos.row=1, layout.pos.col=3))

# See how the variables relate to each other
plot(households)

# Add regression lines to the scatterplots
library(car)
scatterplotMatrix(households, diagonal="histogram", smoother=FALSE)

# 3D scatterplot
# Mac OS X (later versions) requires XQuartz (http://xquartz.macosforge.org/)
scatter3d(income ~ sqft + people, households, residuals=FALSE, fit=FALSE)
scatter3d(income ~ sqft + people, households)

