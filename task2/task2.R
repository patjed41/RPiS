# Patryk Jędrzejczak

library(ggplot2)

rm(list = ls())

# prob(k) = P(Y = k)
prob <- function(k) {
  if (k != 0) {
    1 / (abs(k) * (abs(k) + 1) * (abs(k) + 2))
  }
  else {
    0.5
  }
}

max_pref <- 10000
pref_prob <- cumsum(sapply(1:max_pref, prob))

# funkcja samplująca
samp <- function() {
  x <- runif(1, 0, 1)
  if (x <= 0.5) {
    0
  }
  else  {
    z <- x - 0.5
    if (x > 0.75) {
      z <- x - 0.75
    }
    l <- 1
    r <- max_pref
    while (l < r) {
      s <- as.integer((l + r) / 2)
      if (pref_prob[s] >= z) {
        r <- s
      }
      else {
        l <- s + 1
      }
    }
    if (x > 0.75) {
      l <- -l
    }
    l
  }
}

n = 10000

y <- replicate(n, samp())

# f(k) = P(Y = k)
f <- sapply(-10:10, prob)
sample_f <- sapply(-10:10, function(i) {sum(y == i)}) / n

ggplot() +
  geom_point(aes(x = -10:10, y = f), color = 'blue') +
  geom_point(aes(x = -10:10, y = sample_f), color = 'green')

pref_avg_y <- cumsum(y) / 1:n
pref_median_y <- sapply(1:n, function(i) {median(y[1:i])})

ggplot() +
  geom_line(aes(x = 1:n, y = pref_avg_y), color = 'purple') +
  geom_line(aes(x = 1:n, y = pref_median_y), color = 'red')


