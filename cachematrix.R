## Below are a pair of functions that cache the inverse of a matrix. 
##Function (1) is called "makeCacheMatrix", function (2) is called "cacheSolve".

## (1) "makeCacheMatrix" is a function that creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {  ##sets the value of the matrix
    x <<- y  ##assigns a value to an object in an environment that is different from the current environment
    i <<- NULL  
  }
  get <- function() x  ##gets the value of the matrix
  setsolve <- function(solve) i <<- solve  ##sets solved value of matrix
  getsolve <- function() i  ##gets solved inverse of matrix if it exists
  list(set = set, get = get,  ## the return value of "makeCacheMatrix" is a list of 4 functions
       setsolve = setsolve,
       getsolve = getsolve)
}

## (2) "cacheSolve" is a function that computes the inverse of the "matrix" calculated above by "makeCacheMatrix". 
##If the inverse is already calculated (and matrix is not changed), then "cacheSolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      i <- x$getsolve()    ## Returns a matrix that is the inverse of 'x'
    if(!is.null(i)) {
      message("getting cached data") ## tells you it's retrieving data if inverse matrix is already calculated
      return(i)
    }
    data <- x$get()  ##gets matrix values and places them in "data"
    i <- solve(data, ...)  ##solves the inverse of data
    x$setsolve(i)  ##sets the inverse of data in the "makeCacheMatrix" object
    i
}
