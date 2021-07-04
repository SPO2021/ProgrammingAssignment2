## Below are two functions that cache the inverse of a matrix.

## The "makeCacheMatrix" is a function that creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The "cacheSolve" function finds the inverse of the matrix returned by the function shown above.
## If the inverse has been calculated then it will use the cached data

cacheSolve <- function(x, ...) {   
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
