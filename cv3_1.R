library("seqinr")
library("Biostrings")
library("readxl")

first <- 1
last <- 5
array <- c(1, 2, 3, 0, 5, 6)
a <- c(2, 6, 3, 8, 1, 7)
n <- length(a)


IndexOfMin <- function(array, first, last) {
  index <- first
  for (k in (first + 1):last) {
    if (array[k] < array[index]) {
      index <- k
    }
  }
  return(index)
}
#IndexOfMin(array, first, last)

SelectionSort <- function(a, n) {
  for (i in 1:(n - 1)) {
    j <- IndexOfMin(a, i, n)
    swap(a[i], a[j])
  }  
  return(a)
}  
#SelectionSort(a, n)

RecursiveSelectionSort <- function(a, first, last) {
  if (first < last) {
    index <- IndexOfMin(a, first, last)
    swap(a[first], a[index])
    a <- RecursiveSelectionSort(a, first + 1, last)
  }  
  return(a)
}
#RecursiveSelectionSort(a, first, last)
