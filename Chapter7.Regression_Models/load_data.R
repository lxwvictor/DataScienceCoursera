# Import households survey data, and remove timestamps (irrelevant)
library(reshape2)
households <- read.csv("households.csv",
                       col.names=c("timestamp", "people", "sqft", "income"))
households$timestamp <- NULL
households.melted <- melt(households)
