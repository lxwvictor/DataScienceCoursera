attend <- read.csv("MOOC_7_April.csv")

n <- nrow(attend)
print(attend[runif(1, min=0, max=n),])