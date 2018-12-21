## This file contains 2 functions to create a matrix, and find its
## inverse. Only provide invertible matrices as inputs.

## makeCacheMatrix function returns a list of four functions:
## 1. set the value of a matrix
## 2. get the value of a matrix
## 3. set the inverse of a matrix
## 4. get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInv <- function(invMatrix) {
    inv <<- invMatrix
  }
  getInv <- function() {
    inv
  }
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve function returns the inverse of a matrix, created using
## function makeCacheMatrix. If the inverse of this matrix has already
## been calculated, then the cached value is returned. However, if the
## cached value does not exist, then the inverse is calculated, and
## then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}