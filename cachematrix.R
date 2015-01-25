## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. This script file contains a pair of functions 
## that cache the inverse of a matrix.
#
#    Example:
#    > B = matrix( 
#      +     c(2, 4, 3, 1), 
#      +     nrow=2, 
#      +     ncol=2) 
#    > B
#    [,1] [,2]
#    [1,]    2    3
#    [2,]    4    1
#    > cacheSolve(makeCacheMatrix(B))
#    [,1] [,2]
#    [1,] -0.1  0.3
#    [2,]  0.4 -0.2
#    > 

## The following function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
##
## Return a matrix that is the inverse of 'x'.
##
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
