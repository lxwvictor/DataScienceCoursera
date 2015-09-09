library(quantmod)
amzn = getSymbols("AMZN", auto.assign = FALSE)
sampleTime = index(amzn)

sum(grepl("^2012", sampleTime))
sum(weekdays(sampleTime[grep("^2012", sampleTime)]) == "Monday")
