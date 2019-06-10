## This R code consists of two basic functions 
##1. makeCacheMatrix
##2. cacheSolve
## The objective is to reduce the computation time by storing values like inverse 
## of a matrix which may be used several times during the execution
## Storing the inverse helps in reducing the time and processing power lost in
## recalculating the same inverse value


## makeCacheMatrix 
## creates a list of the following functions
## set() - sets the value of matrix x as per users choice
## get() - gets the matrix for which the inverse is to be cached/calculated
## getinv() - returns the cached inverse of the matrix
## setinv() - computes the invererse of the matrix passed

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve()
##This function when called first checks for the cached inverse of the matrix
## If cached inverse is not available then it returns the computed inverse
## It also caches the computed inverse for use later


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

