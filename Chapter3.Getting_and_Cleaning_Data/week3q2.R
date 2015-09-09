install.packages("jpeg")
library(jpeg)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl, destfile = "./week3q2data.jpg", method = "curl")
resultData <- readJPEG("./week3q2data.jpg", native = TRUE)
quantile(resultData, c(0.3, 0.8))