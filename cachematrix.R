## Below are 2 functions used to cache and calculate inverse of a matrix, respectively.

## makeCacheMatrix creates a special 'matrix' that will store the inverse of matrix, 'i'.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() return(x)
  setinv <- function(inv) i <- inv
  getinv <- function() return(i)
  return(list(set= set, get = get, setinv = setinv, getinv = getinv))
}


## Calculates inverse of special 'matrix' created with 'makeCacheMatrix'.
## If inverse of matrix has already been calculated and stored, 'cacheSolve' will return the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  return(i)
}
