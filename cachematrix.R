## cachematrix.R
## R code to store and retrieve cached results of matrix inversion.
## A list of functions provides the interfaces for manipulating a
## matrix.


## makeCacheMatrix stores a matrix and returns the list of interface functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # initially, don't compurte inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL # reset any cached inverse if the matrix changes
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse<- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes returns the inverse of a matrix that is accessed
## via a list of get/set functions, computing and storing the result only
## when necessary. Additional arguments to solve are passed using ellipsis.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}
