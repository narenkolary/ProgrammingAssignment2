## Two functions will be created under this code, primarilly to enable caching in order
## to avoid potentially time-consuming computations.
## The first (makeCacheMatrix) will create a special "matrix" object that can cache 
## its inverse.
## The second (cacheSolve) will compute the inverse of the object returned by the first
## function. If the inverse has already been calculated, then this function will
## retrive the inverse from the cache.

## This function (makeCacheMatrix) will create a special "matrix" object that can
## cache its inverse as part of the function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function will compute the inverse of the object returned by the function above
## (makeCacheMatrix) and if the inverse has already been calculated, it will retrieve 
## this from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
