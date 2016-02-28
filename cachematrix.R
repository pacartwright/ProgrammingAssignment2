## These functions calculate and cache the result of an inverse 
## matrix operation

## Creates a special "matrix" object that can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the matrix has not changed and the matrix 
## has already been calculated the cached value will be returned,
## if not, the inverse will be calculated.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m        ## Return a matrix that is the inverse of 'x'
}
