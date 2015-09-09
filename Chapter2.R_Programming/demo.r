#setwd("~/R/IDA-MOOC teaching")

a <- 1
a = 1
a == 1

b <- "Singapore"
str(b)

a <- 1:24
a

##### data frame

df <- data.frame(name=character(2), income=numeric(2), age=integer(2), graduate=character(2))
df

df <- data.frame(name=c("Ali", "Boon", "Chandran"),
                 income=c(50000, 40000, 45000),
                 age=c(45, 30, 50),
                 graduate=c("Y", "N", "N"),
                 stringsAsFactors=FALSE)
df
df$name

str(df)

##### dimensions

n <- a
n
length(n)
dim(n)
dim(n) <- c(2,12)
n
dim(n)
?dim
typeof(n)
class(n)
dim(n) <- c(2, 3, 4)
dim(n)
n
typeof(n)
class(n)
length(n)

typeof(df)
class(df)
length(df)

d <- c(a, 25:48)
e <- rbind(a, 25:48)
f <- cbind(a, 25:48)

##### names
df
names(df)
names(df) <- "stud"
df

names(df) <- ### exercise  
df

names(n)
dim(n) <- c(12, 2)
n
names(n) <- c("first", "second")
n
n[1:2]
n[1:3]
colnames(n) <- c("First", "Second")
names(n) <- NULL
rownames(n) <- 1:12
n
l <- dimnames(n)
l
str(l)
dimnames(n) <- NULL

##### subsetting

a
a[3:5]
a[c(3,5)]
dim(n) <- c(6,4)
n
n[3, 4]
n[3:4]
colnames(n) <- c("First", "Second", "Third", "Fourth")
n[3, "Fourth"]

df
df[1,3]
df[2]

df[2,]
df[, 2]
df$stud
df$stud[2]
df[2]$stud
df[2, ]$stud

# exercise: how to get the first and third observations of df
df[c(1, 3), ]
df[c(T, F, T), ]

l
str(l)
l[2]
l[2][2]
l[[2]]
l[[2]][2]
df[1,3]
df[[1]]
df[, 1]
str(df[["stud"]])
str(df["stud"])

### Repetitions
for (x in 1:4) {
  cat("Say")
  cat(x)
}

for (x in c(1, 4)) {
  cat("Say")
  cat(x)
}

### Vectorization

n
# exercise: set d as first 3 columns of last 2 rows of n
d
d[1, 1] * 5
d[2, ] * 5
d * 5
d
d * 6:1
d > 10
n
n > 10
n[n>10]

b <- c("Singapore", "Malaysia", "Indonesia")
b
dim(b) <- c(3,1)
b
c(b,"Thailand", "Vietnam", "Philippines")
rbind(b,"Thailand", "Vietnam", "Philippines")
cbind(b,"Thailand", "Vietnam", "Philippines")

cbind(b, c("Thailand", "Vietnam", "Philippines"))


### Functions

paste("Thailand", "Vietnam", "Philippines")
?paste
paste("Thailand", "Vietnam", "Philippines", sep=",")
paste(b, c("Thailand", "Vietnam", "Philippines"), sep=",", "!")


combine <- function (first, second, insert) {
  a <- paste(first, second, sep=insert)
  return(a)
}
combine("Singapore", "Malaysia", " and ")
combine(insert=" and ", "Singapore", "Malaysia")
combine("Singapore", "Malaysia")

combine <- function (first, second, insert=" and ") {
  a <- paste(first, second, sep=insert)
  return(a)
}
combine("Singapore", "Malaysia")

a
combine("Singapore", a, " number ")


### return to slide