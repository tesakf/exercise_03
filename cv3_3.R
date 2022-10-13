library("seqinr")
library("Biostrings")
library("readxl")

CZKChange2 <- function(M, array) {
  num_of_coins <- 0
  
  for (i in 1:length(array)) {
    if (M%/%array[i] >= 1) {
      num_of_coins <- num_of_coins + M%/%array[i]
      M <- M - M%/%array[i]*array[i]
    } 
  }
  return(num_of_coins)
}

CZKChange2(158, c(25, 5, 1))

