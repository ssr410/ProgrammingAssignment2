## Put comments here that give an overall description of what your
## functions do
## We create two functions which caches the
## inverse of a matrix.  Matrix inversion is
## computationally time consuming for large
## size matrices.  Sometimes in code (and especially in loops),
## the inverse of a matrix need only be computed once.  
## To avoid recomputing the inverse and generating the
## same result repeatedly, we can simply compute the
## result once.  If we try to recompute the inverse again,
## we have already computed this already and so we should
## just return this pre-computed result.
##


## Write a short comment describing this function
## makeCacheMatrix:
## To facilitate this caching, we first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.

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
## Once you create the above matrix, you use the cacheSolve
## function to compute the inverse and cache the result
##
## If you try using cacheSolve again on the same special
## matrix, then the pre-computed result is obtained, thus
## avoiding any recomputation.  An informative message
## will be shown in the command prompt when the pre-computed
## result is returned instead.

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