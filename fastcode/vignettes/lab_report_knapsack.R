## ------------------------------------------------------------------------
library("fastcode")
library("parallel")
set.seed(42)
n <- 2000
knapsack_objects <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE),
v=runif(n = n, 0, 10000)
)
knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
system.time(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500))

## ------------------------------------------------------------------------
knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE)
system.time(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE))

## ------------------------------------------------------------------------
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
system.time(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))

## ------------------------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
system.time(greedy_knapsack(x = knapsack_objects[1:800,], W = 3500))

## ------------------------------------------------------------------------
system.time(knapsack_dynamic(x = knapsack_objects[1:500,], W = 3500))

## ------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects1 <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE),
v=runif(n = n, 0, 10000)
)
system.time(greedy_knapsack(x = knapsack_objects1[1:1000,], W = 3500))

