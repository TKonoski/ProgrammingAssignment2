##  makeCacheMatrix reates a special "matrix" object that can cache its inverse.
##  cacheSolve computes the inverse of a matrix returned by
##  makeCacheMatrix.  If the inverse has already been calculated,
##  cacheSolve will retrieve the inverse from the cache.


## makeCacheMatrix - Creates a special "matrix" object that can cache its inverse
## which returns a list containing a function to:
## 1. Set the the matrix
## 2. get the the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setInverse <- function(solve) m <<- solve
getInverse <- function() m
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## Computes the inverse of the special "matrix" return by makeCacheMatrix
## If the reverse has been calculated, cacheSolve will retrieve the inverse from cache
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data...")
    return(m)
  } 
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
