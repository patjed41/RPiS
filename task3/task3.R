library(ggplot2)

rm(list = ls())

n <- 64 # liczba zmiennych
m <- 64 # liczba nawiasów
k <- 10 # liczba zamiennych w nawiasie

phi <- t(sapply(1:m, function(index) {
  sample(1:n, k) * sample(c(-1, 1), k, replace = T)
}))

samples_per_clause <- 10 # liczba losowanych przypisań spełniających wybrany nawias
runs <- 50 # liczba generowanych wartości (liczba spełniających przypisań / 2^n)

probabilities <- sapply(1:runs, function(run_index) {
  1 + sum(sapply(2:m, function(clause_index) {
    mean(sapply(1:samples_per_clause, function(sample_index) {
      x <- sample(c(-1, 1), n, replace = T)
      for (variable in phi[clause_index,]) {
        if (x[abs(variable)] * variable < 0) {
          x[abs(variable)] <- -x[abs(variable)]
        }
      }
      sum(apply(phi[(1:clause_index),], 1, function(row) {
        all(x[abs(row)] * row > 0)
      })) == 1
    }))
  }))
}) / 2^k

avg <- mean(probabilities)
variance <- var(probabilities)

ggplot(data.frame(probabilities), aes(x = probabilities)) + 
  geom_histogram(binwidth = 0.0001, color = "purple")