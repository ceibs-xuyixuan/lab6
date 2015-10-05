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
            for(i in 1 : (2^n-1)){
                  sum_wv$w[i] <- sum(x$w[(which(a[,i] == 1))])  #sum(w)
                  sum_wv$v[i] <- sum(x$v[(which(a[,i] == 1))])  #sum(v)
                  elem[[i]] <- list(which(a[,i]  == 1))
            }
            kna <- max(sum_wv$v[which(sum_wv$w <= W)])
            ele <- elem[[which(kna == sum_wv$v)]][[1]]
      }else{ 
            cl <- detectCores(logical=FALSE)
            
            sum_wv$w <- unlist(mclapply(1: (2^n-1), FUN=function(i){return(sum(x$w[(which(a[,i] == 1))]))}, mc.cores = cl))
            sum_wv$v <- unlist(mclapply(1: (2^n-1), FUN=function(i){return(sum(x$v[(which(a[,i] == 1))]))}, mc.cores = cl))
            elem<- mclapply(1:(2^n-1), FUN=function(i){which(a[,i]  == 1)},mc.cores = cl)
            kna <- max(sum_wv$v[which(sum_wv$w <= W)])
            ele <- elem[[which(kna == sum_wv$v)]]
      }
      return(list(value = kna, elements = ele))
}
