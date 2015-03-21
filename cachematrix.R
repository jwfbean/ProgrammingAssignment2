## These functions are the beginning of "object oriented programming" in R.
## makeCacheMatrix(x) returns a list containing a cached version of matrix x 
## with a getinverse function that will return a related cached matrix (presumably solved)
## and a setinverse function that will cache the related matrix (presumably solved)
## 
## cacheSolve actually takes a cached matrix and solves it if it has not been solved
## already, storing the solution in the cached matrix.
##


## Return a list containing setters and getters for the cached matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  cachedmatrix <- x
  cachedinverse <- NULL
  set <- function(y) {
    cachedinverse <<- NULL
    cachedmatrix <<- y
  }
  get <- function() cachedmatrix
  setinverse <- function(inverse) cachedinverse <<- inverse
  getinverse <- function() cachedinverse
  list (set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}
## Return the solution of the cached matrix, calculating if necessary
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
