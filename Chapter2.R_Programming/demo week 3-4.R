### Factors
dff <- data.frame(name=c("Ali", "Boon", "Chandran"),
                 income=c(50000, 40000, 45000),
                 age=c(45, 30, 50),
                 graduate=c("Y", "N", "N"),
                 rank=c("4", "3", "3"))

dff
str(dff)
bonus <- dff$income * .05 * dff$rank
bonus

bonus <- dff$income * .05 * as.numeric(dff$rank)
bonus

levels(dff$rank)
factor(dff$rank)
levels(dff$name)
as.numeric(dff$name[1])
as.numeric(dff$rank)

bonus <- dff$income * .05 * as.numeric(as.character(dff$rank))

df <- data.frame(name=c("Ali", "Boon", "Chandran"),
                 income=c(50000, 40000, 45000),
                 age=c(45, 30, 50),
                 graduate=c("Y", "N", "N"),
                 rank=c("4", "3", "3"),
                 stringsAsFactors = FALSE)

as.numeric(df$name[1])
as.numeric(df$rank)
df$rank <- as.numeric(df$rank)

########################

### Simulations

# modelling a bell curve (i.e. normal), e.g. salary
set.seed(12345)
avg <- 50000
std <- 1000
salary <- rnorm(1000, avg, std)
head(salary)
plot(salary)
hist(salary)
mean(salary)
sd(salary)

#seed vs no seed
salary2 <- rnorm(1000, avg, std)
hist(salary2)
set.seed(12345)
hist(rnorm(1000, avg, std))

#modeling theoretical equation, with noise
bonus <- salary * .1
plot(salary, bonus)
e <- rnorm(length(bonus), 0, 50)
bonus <- salary * .1 + e
plot(salary, bonus)

# other models eg Poisson e.g. bus arrivals
rate <- 3
bus <- rpois(1000, rate)
plot(bus)
hist(bus)
table(bus)
hist(bus, (min(bus)-1):max(bus))
hist(bus, ((min(bus)-1):max(bus))+.5)
plot(table(bus))


########################

### apply family

#statistics of bus arrivals
routes <- bus
dim(routes) <- c(250, 4)
colnames(routes) <- c(80, 145, 82, 113)
head(routes)
mean(routes)
colMeans(routes)
head(rowMeans(routes))

sd(routes)
routes.sd <- 0
for (x in 1:dim(routes)[2])
  routes.sd[x] <- sd(routes[, x])
routes.sd
apply(routes, 2, sd)

#lapply and sapply: finding the length of the longest word
#in names of US states
?state
str(state.name)
all.char <- nchar(state.name)
max(all.char)
state.name[all.char == max(all.char)]

strsplit("North Carolina", " ")
split.name <- strsplit(state.name, " ")
str(split.name)
nchar(split.name[[50]])
nchar(split.name[[48]])
all.char <- nchar(split.name)
str(all.char)
all.char[48]  # vectorization unpredictable in lists

l.all.char <- lapply(split.name, nchar)
str(l.all.char)

max(all.char)
max(l.all.char)
max(unlist(l.all.char))

l.max.char <- lapply(split.name, function(x) max(nchar(unlist(x))))
str(l.max.char)
max(l.max.char)
s.max.char <- sapply(split.name, function(x) max(nchar(unlist(x))))
str(s.max.char)
max(s.max.char)
max(unlist(l.max.char))

state.name[s.max.char == max(s.max.char)]
which(s.max.char == max(s.max.char))

#mapply example: Scrabble letter distributions
tilecount <- c(9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2, 6, 8, 2, 1, 6, 4, 6, 
               4, 2, 2, 1, 2, 1)
rep("a", tilecount[1])
tiles <- c(rep("a", tilecount[1]), rep("b", tilecount[2]),
         rep("c", tilecount[3]))
tiles
tiles <- sapply(letters[1:length(tilecount), rep, tilecount])
rep(letters[1], tilecount)

#multivariate needed: use mapply
tiles <- mapply(rep, letters[1:length(tilecount)], tilecount)
tiles
tiles <- unlist(tiles)
tiles

sample(tiles, 7)

#grouped lists
head(tiles)
bag <- data.frame(tile = tiles,
  vowel=tiles %in% c("a", "e", "i", "o", "u"),
  stringsAsFactors=FALSE)

bag[1:20, ]

separated.bag <- split(bag, bag$vowel)
str(separated.bag)
#exercise: count how many vowels and how many consonants?
sapply(separated.bag, nrow)
#exercise: count how many tiles of each letters?
sapply(split(bag, bag$tile), nrow)
table(bag$tile)

#multiple category group
df
interaction(df$graduate, df$rank)
table(interaction(df$graduate, df$rank))
s.df <- split(df, interaction(df$graduate, df$rank))
s.df
s.df <- split(df, interaction(df$graduate, df$rank), drop=TRUE)
s.df


# selective apply
head(routes)
groups <- rep(1:50, each=5, times=4)
groups[1:100]
groups[201:300]
arrivals <- tapply(routes, groups, sum)
arrivals
routes[1:10,]
sum(routes[1:5, ])

df
#find average income by rank
tapply(df$income, df$rank, mean)

########################

### timing
plot(salary, bonus)
system.time(bonus <- salary * .1 + e)
bonus.for <- numeric(length(salary))
system.time(for (x in seq_along(salary))
  bonus.for[x] <- salary[x] * .1 + e[x])

# if you don't have the package, type
# install.packages("microbenchmark")
require(microbenchmark)
microbenchmark(bonus <- salary * .1 + e,
               for (x in seq_along(salary))
                 bonus.for[x] <- salary[x] * .1 + e[x]
               )

# using proc.time
first <- proc.time()
rd <- rnorm(100000, avg, std)
second <- proc.time()
plot(rd)
third <- proc.time()

second - first
third - first