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
