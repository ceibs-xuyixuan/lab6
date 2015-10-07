#' The only solution that is guaranteed to give a correct answer in all 
#' situations for the knapsack problem
#' is using brute-force search, i.e. going through all possible alternatives 
#' and return the maximum value
#' found. This approach is of complexity O(2 n ) since all possible 
#' combinations 2 n needs to be evaluated.
#' @param x A matrix with name v and w.
#' @param W A number.
#' @param parallel Multicore control.
#' @return A list with value and elements.
#' @examples
#' data(knapsack_objects)
#' knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
#' @export
knapsack_brute_force <- function(x, W, parallel = FALSE){
  stopifnot(names(x) == c("w","v")&& is.data.frame(x))
  n <- nrow(x)
  i <- 1
  a <- matrix(as.integer(intToBits(1:(2^n-1))), nrow = 32)
  sum_wv<- NULL
  elem <- NULL
  ele <- NULL
  kna <- NULL
  if(parallel == FALSE){
    sum_wv$w <- unlist(lapply(1: (2^n-1),  FUN=function(i){return(sum(x$w[(which(a[,i] == 1))]))}))
    sum_wv$v <- unlist(lapply(1: (2^n-1),  FUN=function(i){return(sum(x$v[(which(a[,i] == 1))]))}))
    elem<- lapply(1:(2^n-1), FUN=function(i){which(a[,i]  == 1)})
  }else{
    cl <- detectCores(logical=FALSE)
    sum_wv$w <- unlist(mclapply(1: (2^n-1), FUN=function(i){return(sum(x$w[(which(a[,i] == 1))]))}, mc.cores = cl))
    sum_wv$v <- unlist(mclapply(1: (2^n-1), FUN=function(i){return(sum(x$v[(which(a[,i] == 1))]))}, mc.cores = cl))
    elem<- mclapply(1:(2^n-1), FUN=function(i){which(a[,i]  == 1)},mc.cores = cl)
  }
  kna <- max(sum_wv$v[which(sum_wv$w <= W)])
  ele <- elem[[which(kna == sum_wv$v)]]
  return(list(value = kna, elements = ele))
}
