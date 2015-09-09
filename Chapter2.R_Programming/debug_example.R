int.div <- function (x, div) {
  a <- x %/% div
  b <- x %% div
  return (data.frame(dividend=x, divisor=div,
                     quotient=a, remainder=b))
}

a <- runif(10)
a
b <- trunc(a*100)
b

outcome1 <- int.div(b, 5)
outcome2 <- int.div(14, 0:5)
outcome3 <- int.div(10:20, 1:5)
outcome4 <- int.div(as.character(b), 5)
outcome5 <- int.div(1000, b)


