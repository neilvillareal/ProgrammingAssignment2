rm(list = ls())  

source('cachematrix.R')

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

matrixParam <- hilbert(8);

result <- makeCacheMatrix(matrixParam)


inverse <- cacheSolve(result)

result$setInverse(inverse)



