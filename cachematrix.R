## Put comments here that give an overall description of what your functions do

# This returns a list of functions to: set a matrix, get a matrix, set the inverse of a matrix, get the inverse of a matrix
# Note: I found that this function must be run before trying to "cacheSolve" a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


# This function takes as argument a list of 4 functions (as returned by makeCacheMatrix)
# This function tries to get the inverse of a matrix. If it's not null, that means the inverse has been calculated:
# the function gets the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
