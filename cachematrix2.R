## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initially set to NULL as first step
  inv <- NULL
  # Sets the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Gets the matrix 
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  # Sends into a list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function.

cacheSolve <- function(x, ...) {
  # Get current state of the inverse and see if it has been processed
  inv <- x$getInverse()
  # If yes
  if (!is.null(inv)) {
    # Return the  inverse
    message("getting cached data")
    return(inv)
  }
  # If No
  # Get the matrix
  mat <- x$get()
  # Finds the inverse
  inv <- solve(mat, ...)
  # Caches the result
  x$setInverse(inv)
  # Returns the result
  inv
}