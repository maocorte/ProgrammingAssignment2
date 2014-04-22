## The following function creates a special "matrix",
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
## If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value to NULL
  res <- NULL
  
  # setter for matrix value
  set <- function(y){
    x <<- y
    res <<- NULL
  }
  
  # getter for matrix vale
  get <- function() x
  
  # setter for matrix inverse
  setInverse <- function(inverse) res <<- inverse
  
  # getter for matrix inverse
  getInverse <- function() res
  
  # return a list with object definition
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached
  res <- x$getInverse()
  if(!is.null(res)){
    # value already cached so it is returned
    message("getting cached data")
    return(res)
  }
  # value not cached, so we get the matrix into data
  data <- x$get()
  # compute the inverse
  res <- solve(x, ...)
  # then cache the computed inverse
  x$setInverse(res)
  # and return it
  res
}
