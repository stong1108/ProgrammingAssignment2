## This file is Stephanie Tong's submittal of Programming Assignment 2 
##of the R Programming Coursera class. 

## The first function, makeCacheMatrix(x = matrix()), creates a list of
## functions for retrieving & setting a matrix and/or its inverse. 
## This function does not calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) matinv <<- inv
  getinv <- function() matinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The second function, cacheSolve(x,...) first searches for a stored
## matrix inverse value.
## A notification will appear if the matrix inverse value has been set 
## previously. If this value has not been set, then the function will
## calculate and set the matrix inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
}
