# This module defines a special kind of matrix, that caches the result of the
# inverse matrix calculation.
#
# Matrix inversion is usually a costly computation and, thus, caching its
# result instead of computing it repeatedly is a good strategy.
makeCacheMatrix <- function(x = matrix()) {
  # This function is a matrix wrapper, that allows for caching the matrix inversion.
  #
  # Args:
  # x: The matrix to be wrapped.
  #
  # Returns:
  # A list representing the special matrix, containing functions to set and
  # get the matrix and inverse matrix values.
  cached.inverse <- NULL
  # When setting new values to the matrix, the cached inverse must be reset
  set <- function(y) {
    x <<- y
    cached.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached.inverse <<- inverse
  getinverse <- function() cached.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  # This function calculates the inverse of the matrix created with the function makeCacheMatrix.
  #
  # It checks to see if the inverse has already been calculated. If so, it gets
  # the inverse matrix from the cache. Otherwise, it calculates the inverse of
  # the matrix and sets the value of the inverse in the cache.
  #
  # Args:
  # x: The wrapped matrix, created with makeCacheMatrix.
  #
  # Returns:
  # The inverse of the given matrix.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Using cached data")
  } else {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
  }
  inverse
}
