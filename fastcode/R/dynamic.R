knasack_dynamic<-function(x, W){
  stopifnot(names(x) == c("w","v")&& is.data.frame(x))
  A <- matrix(rep(0,(W + 1)*(nrow(x)+1)),ncol = (W+1))
  b <- NULL
  j <- 1
  for (j in 1:length(x$w)) {
    Y <- 1
    for (Y in 1:W) {
      if (x$w[j] > Y){
        A[j+1, Y+1] <- A[j, Y+1]
      }
      else{
        A[j+1, Y+1] <- max( A[j, Y+1], x$v[j] + A[j, Y- x$w[j]+1])
      }
    }
  }
  element <- NULL
  a <- A[nrow(A), ncol(A)]
  i <- length(x$w)
  y <- W
  for(i in length(x$w):1){
    if(A[i+1,y+1] != A[i,y+1]&&A[i+1,y+1]!=0){
      element <- c(element,i)
      y <- y-x$w[i]
    }
  }
  return(list(value = A[j+1, Y+1], elements = sort(element,decreasing = FALSE)))
}