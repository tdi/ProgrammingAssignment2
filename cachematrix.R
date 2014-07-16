## Put comments here that give an overall description of what your
## functions do

## This function creates a vector containing functions to set matrix, get the matrix, set its
## iverset and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates an inverse of a matrix, or returns a cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        # if we have a value cached, get it and return
        if (!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
        }
        # otherwise calculate and cache for the future
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
