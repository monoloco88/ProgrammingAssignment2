## Programming Assignment 2
## These functions cache a matrix and finds the inverse of that matrix.

## The first function can be used to create an object that caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_x <<- inverse
  getInverse <- function() inv_x
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function gets the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matrix_x <- x$get()
    inverse <- solve(matrix_x, ...)
    x$setInverse(inverse)
    inverse
}
