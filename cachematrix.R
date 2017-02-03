## There are two functions in this script, namely 'makeCacheMatrix' & 'CacheSolve'. These functions basically find an inverse of 
## the matrix(argument). Once the inverse is found, it can be looked up in the cache rather than recomputing. These functions are
## extremely helpful when we want to use the inverse of a matrix of large dimension in our workspace again and again (of the same matrix).

## This is 'makeCacheMatrix' function. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This is 'cacheSolve' function. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
   ## Return a matrix that is the inverse of 'x'
}
