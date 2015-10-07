library("fastcode")
library("parallel")
data(knapsack_objects)
context("correct function")
test_that("correct function", {
      set.seed(42)
      n <- 2000
      knapsack_objects <-
            data.frame(
                  w=sample(1:4000, size = n, replace = TRUE),
                  v=runif(n = n, 0, 10000)
            )
      expect_that(knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)$elements, 
                  equals(c(5,8)))
})
context("x must be a matrix with v and w")
test_that("x must be a matrix with v and w", {
      set.seed(42)
      n <- 2000
      knapsack_objects <-
            data.frame(
                  w=sample(1:4000, size = n, replace = TRUE),
                  v=runif(n = n, 0, 10000)
            )
      expect_that(names(knapsack_objects), equals(c("w","v")))
})