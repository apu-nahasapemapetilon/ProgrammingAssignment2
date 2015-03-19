##  Together, makeCacheMatrix() and cacheSolve cache the inverse of a matrix

##  makeCacheMatrix() creates a matrix that caches the inverse of the matrix supplied

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##  cacheSolve() returns the inverse of the matrix created by makeCacheMatrix(). 
##  cacheSolve computes the inverse of a matrix if the inverse has not already been cached, 
##  otherwise the inverse is retrieved from cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
