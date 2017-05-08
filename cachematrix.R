## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix - creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse_x <- NULL
  
  ## Set the matrix
  set<-function(y){
    x <<- y
    inverse_x <<- NULL
  }
  
  ## Return the matrix
  get <- function() x
 
  ## Set the inverse of the matrix
  setInverse <- function(inverse) inverse_x <<- inverse
  
  ## Return the inverse of the matrix
  getInverse <- function() inverse_x
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve - computes the inverse of the "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Get inverse of 'x'
  inverse_x <- x$getInverse()
  
  ## Return the inverse if its already valid
  if( !is.null(inverse_x) ) {
    message("getting cached data")
    return(inverse_x)
  }
  
  ## Get the matrix so inverse can be computed
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverse_x <- solve(data) %*% data
  
  ## Set the inverse of the matrix
  x$setInverse(inverse_x)
  
  ## Return the inverse of the matrix
  inverse_x 
  
}
