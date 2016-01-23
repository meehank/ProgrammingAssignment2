## makeCacheMatrix and cachesolve implement a method to avoid inverting a matrix repeatedly.
## cachesolve acts upon a special "CacheMatrix" list (derived from an original matrix) that caches
## the inverse of the original matrix with the original matrix.

## After the inverse of the matrix in the "CacheMatrix" is found once, it is stored in the "CacheMatrix"
## so that subsequent calls to cachesolve do not require computing the inverse; the inverse is just recalled
## from the cache.
##
## The <<- operator assigns a value to an object in an environment that is different from the current environment.
## That part is still a bit mysterious to me..


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}