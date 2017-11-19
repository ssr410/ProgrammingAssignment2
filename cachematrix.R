## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL() ## Initially set the inverse to NULL
  
  ## Construct a function to set the matrix (but not the inverse)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##The following gets the matrix (not its inverse)
  get <- function() x
  
  ## Manually set the inverse
  setInverse <- function(inverse) m <<- inverse
  
  ##Get the inverse
  getInverse <- function() m
  
  ##Lists all results until now
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix 'x'
  
  ##Shows the current stage of calculations, whether the inverse has been computed.
  m <- x$getInverse()
  
  ##If the inverse has been computed then gets the inverse via following function.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If the inverse has not been computed, then the following gets the matrix and 
  ## solves/computes its inverse
  data <- x$get()
  m <- solve(data)
  
  ##Cache this result
  x$setInverse(m)
  
  ##Return the new result
  m
}