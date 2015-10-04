set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
greedy_knapsack <- function(x,W){
  stopifnot(names(x) == c("w","v")&& is.data.frame(x))
  average <- NULL
  i <- 1
  for(i in 1 : nrow(x)){
    x$a[i] <- x$v[i]/x$w[i]
  }
  j <- 1
  y <- order(x$a, decreasing = TRUE)
  for(j in 1 : nrow(x)){
    if(sum(x$w[y[1:j]])<=W){
      value <- sum(x$v[y[1:j]])
      ele <- y[1:j]
    }
  }
  return(list(value = value, elements = ele))
}
greedy_knapsack(x = knapsack_objects[1:800,], W= 3500)