## Programming Assignment 2
## These functions cache a matrix and finds the inverse of that matrix.

## The first function can be used to create an object that caches a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL ##define inverse variable
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  } ##function to change the matrix that is stored
  get <- function() x ##function to return cached matrix
  setInverse <- function(inverse) inv_x <<- inverse ##function to store inverse
  getInverse <- function() inv_x ##function to return cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function gets the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse() ##grabs the cached inverse
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv) ##returns the cached inverse
    }
    matrix_x <- x$get() ##retrieves the cached matrix
    inverse <- solve(matrix_x, ...) ##calculates the inverse of the cached matrix
    x$setInverse(inverse) ##stores inverse object
    inverse
}
