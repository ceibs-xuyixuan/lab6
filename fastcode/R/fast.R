knapsack_brute_force <- function(x, W){
  stopifnot(names(x) == c("w","v")&& is.data.frame(x))
  n <- nrow(x)
  i <- 1
  a <- matrix(as.integer(intToBits(1:(2^n-1))), nrow = 32)
  sum_wv<- NULL
  elem <- NULL
  ele <- NULL
  for(i in 1 : (2^n-1)){
    sum_wv$w[i] <- sum(x$w[(which(a[,i] == 1))])  #sum(w)
    sum_wv$v[i] <- sum(x$v[(which(a[,i] == 1))])  #sum(v)
    elem[[i]] <- list(which(a[,i]  == 1))
  }
  kna <- max(sum_wv$v[which(sum_wv$w <= W)])
  ele <- elem[[which(kna == sum_wv$v)]][[1]]
  return(list(value = kna, elements = ele))
}
