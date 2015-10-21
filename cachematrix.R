# This code implements cachematrix that computes and stores the inverse of a matrix.
# There are two functions in this file: makeCacheMatrix and cacheSolve.
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been calculated 
#    (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## The function makeCacheMatrix takes a matrix as argument
# and returns a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
# set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
# get the value of the matrix
  get <- function() x
  
# set the value of the inverse
  setinverse <- function(inverse) m <<- inverse
  
# get the value of the inverse
  getinverse <- function() m
  
  # retruns a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse 
# in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  # first check to see if the inverse has already been calculated.
  m <- x$getinverse()
  if(!is.null(m)) { 
    # If so, get the inverse from the cache and skip the computation.
    message("getting cached data")
    return(m)
  }
  
# Otherwise, calculate the inverse of the data 
  data <- x$get()
  m <- solve(data, ...)
  
  # set the value of the inverse in the cache via the setinverse function.
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
