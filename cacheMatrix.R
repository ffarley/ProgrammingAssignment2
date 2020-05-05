## makeCacheMatrix creates a special Matrix object that can cache its inverse.
## cachSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    get <- function()
      x
    setinv <- function(inverse)
      inv <<- inverse
    getinv <- function()
      inv
    list(
      set = set,
      get = get,
      setinv = setinv,
      getinv = getinv)
  }

}


## cacheSolve calculates the inverse of the special matrix created by 
## makeCacheMatrix


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
      message("getting cached result")
      return(inv)
  }
  
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
  
}
