## The combination of these two functions cache the inverse of a matrix.
## The 'makeCacheMatrix' function creates a list of functions to cache the 
## inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 'cacheSolve' function tests to see if the inverse of the matrix is already
## stored, and returns that value along with a message, or if the inverse is not 
## already cached, this function uses solve() to obtain the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("Retrieving cached data.. Please hold.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv 
}
