# Patryk Jędrzejczak

library(ggplot2)

rm(list = ls())

# dla n = 10^6 też działa sensownie szybko
# n = 1000000

n = 100000
d = 372

birth_data <- read.csv(url("https://raw.githubusercontent.com/eryxcc/rpis2018/master/lab1/us_births_69_88.csv"))
birth_nums <- birth_data$births
max_birth <- max(birth_nums)

u <- sample(max_birth, 40 * n, replace = TRUE)
k <- sample(d, 40 * n, replace = TRUE)
k_u_index <- 1
birth <- vector("integer", 30 * n)

for (i in 1:(30 * n)) {
  while (u[k_u_index] >= birth_nums[k[k_u_index]]) {
    k_u_index <- k_u_index + 1
  }
  birth[i] <- k[k_u_index]
  k_u_index <- k_u_index + 1
}

values <- vector("integer", n)
last_occurence <- vector("integer", d)
birth_index <- 1

for (i in 1:n) {
  distinct <- 0
  while (last_occurence[birth[birth_index]] < i) {
    last_occurence[birth[birth_index]] <- i
    birth_index <- birth_index + 1
    distinct <- distinct + 1
  }
  values[i] <- distinct
  birth_index <- birth_index + 1
}

histogram_data <- data.frame(values)

histogram <- ggplot(histogram_data, aes(x = values)) + 
  geom_histogram(binwidth = 1, color = "green")

histogram

