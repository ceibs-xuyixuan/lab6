#' Greedy heuristic
#' @param x A matrix with name v and w.
#' @param W A number.
#' @return A list with value and elements.
#' @examples
#' library("parallel")
#' data(knapsack_objects)
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' @export
greedy_knapsack <- function(x,W){
      stopifnot(names(x) == c("w","v")&& is.data.frame(x))
      average <- NULL
      cl <- detectCores(logical=FALSE)
      x$a <- unlist(mclapply(1:nrow(x), FUN=function(i){x$v[i]/x$w[i]}, mc.cores = cl))
      y <- order(x$a, decreasing = TRUE)
      sumw <- unlist(mclapply(1 : nrow(x), FUN = function(j){if(sum(x$w[y[1:j]])<=W)
      {sum=sum(x$v[y[1:j]])}}, mc.cores = cl))
      ele <- y[1:which.max(sumw)]
      return(list(value = max(sumw), elements = ele))
}