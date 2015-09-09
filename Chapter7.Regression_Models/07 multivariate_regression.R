source("load_data.R")

# Visualising regression with 2 variables
model <- lm(income ~ sqft + people, households)
# Mac OS X (later versions) requires XQuartz (http://xquartz.macosforge.org/)
library(car)
scatter3d(income ~ sqft + people, households, residuals=FALSE, fit=FALSE)
scatter3d(income ~ sqft + people, households)

# Everything works the same way
summary(model)
coefficients(model)
confint(model)
