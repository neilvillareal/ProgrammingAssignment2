

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  # initialize inverse matrix
  inverseMatrix <- NULL
  
  #
  getter <- function() { return(x) }
  
  setter <- function(m)  {
    x <<- m
    inverseMatrix <<- NULL
  }
  
  inverseSetter <- function(inverseParam){
    inverseMatrix <<- inverseParam
  }
  
  inverseGetter <- function(){
    return(inverseMatrix)
  }
  
  result <- list(set = setter, 
                 get = getter, 
                 setInverse = inverseSetter,  
                 getInverse = inverseGetter)
  
  return(result)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x) {
  
  # get the inverse from 'special matrix'
  inverseMatrix <- x$getInverse()
  
  # return inverse matrix if already set
  if (!is.null(inverseMatrix)) {
      message('have an inverse matrix') 
      return(inverseMatrix)
  }
  
  data <- x$get()
  
  inverse <- solve(data)
  
  x$setInverse(inverse)
  
  return(inverse)
}