# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. 
# makeCacheMatrix creates a special matrix to cache the inverse of itself and stores it. In particular, 
# a) set the values of the matrix
# b) get the values of the matrix
# c) set the values of inverse matrix
# d) get the values of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function then retrieves the cache inverse matrix if it already exists, otherwise it computes it and prints it to the screen.  

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
 inv
}

