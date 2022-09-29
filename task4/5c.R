library(ggplot2)

rm(list = ls())

f <- matrix(data = c(17508, 11642,  3308,  3131,  2911,  2205,  1852, 1235,
                     17672,  9318,  4865,  3259,  3029,  2479,  1606, 3259),
            nrow = 2,
            byrow = TRUE)

r <- apply(f, 1, sum)
c <- apply(f, 2, sum)
N <- sum(r)

S <- sum(sapply(1:nrow(f), function(row) {
  sum(sapply(1:ncol(f), function(column) {
    X <- r[row] * c[column] / N
    (f[row, column] - X)^2 / X
  }))
}))

p_value <- pchisq(S, df = (nrow(f) - 1) * (ncol(f) - 1), lower.tail = FALSE)