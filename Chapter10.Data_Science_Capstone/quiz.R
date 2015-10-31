library(jsonlite)

## Q5: Find percentage of 5-star reviews.

nLine = 10000
reviews = lapply(readLines("Data/yelp_academic_dataset_review.json", nLine), fromJSON)
reviewL = unlist(reviews) # flatten list of reviews
colNames = names(reviewL)[1:10] # get column names
# convert to matrix of nLine rows by 10 columns in row-major format
# every value in review list will be converted to string
reviewM = matrix(reviewL, nLine, 10, byrow = T, list(NULL, colNames))
# convert to data frame
reviewF = data.frame(reviewM, stringsAsFactors = F)
# convert columns back to integers
intCol = c("votes.funny", "votes.useful", "votes.cool", "stars")
for (i in intCol) reviewF[i] = as.integer(unlist(reviewF[i]))
reviewF$date = as.Date(reviewF$date)
reviewF$type = as.factor(reviewF$type)
mean(reviewF$stars == 5)

## Q7: Find percentage of free Wi-Fi out of businesses with the attribute "Wi-Fi".

business = lapply(readLines("Data/yelp_academic_dataset_business.json"), fromJSON)
businessL = unlist(business) # flatten list
# get indices of businesses with Wi-Fi attribute
indices = names(businessL) == "attributes.Wi-Fi"
mean(businessL[indices] == "free")

## Q10: Find the user with over 10000 funny compliments.

users = lapply(readLines("Data/yelp_academic_dataset_user.json", 100000), fromJSON)
# get vector of funny compliments
funny = sapply(users, function (x) { i = x$compliments$funny; if (is.null(i)) 0 else i })
users[[which(funny  > 10000)]]$name

## Q11: Fisher test.

# logical vector X: true if user has > 1 fan
X = sapply(users, function (x) { if (x$fans > 1) T else F })
# logical vector Y: true if user has > 1 funny compliment
Y = sapply(users, function (x) { i = x$compliments$funny; if (is.null(i) || i <= 1) F else T })
# test independence of X and Y
fisher.test(table(X, Y))
