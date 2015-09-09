fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"

download.file(fileUrl, destfile = "./getdata-wksst8110.for", method = "curl")

data <- read.fwf(file = "./getdata-wksst8110.for", widths = c(10,9,4,9,4,9,4,9,4),
                 skip = 4)

sum(data$V4)
