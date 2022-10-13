library("seqinr")
library("Biostrings")
library("readxl")

CZKChange <- function(M) {
  num_of_coins <- 0
  num_of_50 <- 0
  num_of_20 <- 0
  num_of_10 <- 0
  num_of_5 <- 0
  num_of_1 <- 0
  
  #Give the integer part of M/50 50 CZK coins to customer.
  #Let remainder be the remaining amount due the customer.
  
  if (M%/%50 >= 1) {
    num_of_50 <- M%/%50
    M <- M - num_of_50*50
  }
  
  #Give the integer part of remainder/20 20 CZK coins to customer.
  #Let remainder be the remaining amount due the customer. 
  
  if (M%/%20 >= 1) {
    num_of_20 <- M%/%20
    M <- M - num_of_20*20
  }
  
  #Give the integer part of remainder/10 10 CZK coins to customer.
  #Let remainder be the remaining amount due the customer.
  
  if (M%/%10 >= 1) {
    num_of_10 <- M%/%10
    M <- M - num_of_10*10
  }
  
  #Give the integer part of remainder/5 5 CZK coins to customer.
  #Let remainder be the remaining amount due the customer.
  
  if (M%/%5 >= 1) {
    num_of_5 <- M%/%5
    M <- M - num_of_5*5
  }
  
  #Give remainder in 1 CZK coins to customer.
  
  if (M%/%1 >= 1) {
    num_of_1 <- M%/%1
    M <- M - num_of_1*1
  }
  
  num_of_coins <- num_of_50 + num_of_20 + num_of_10 + num_of_5 + num_of_1
  return(num_of_coins)
}

CZKChange(158)

