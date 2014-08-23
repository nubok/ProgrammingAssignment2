## In this file two functions are defined:
##
## makeCacheMatrix: creates a matrix that caches its inverse to avoid
## having to recalculate it too often.
##
## cacheSolve: Pass a matrix created by makeCacheMatrix to it 
## to get its inverse. It will use a cached inverse if possible.

## As the assignment tells us (see 
## https://class.coursera.org/rprog-006/human_grading/view/courses/972578/assessments/3/submissions:
## for reference):
## "makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse."
##
## Additionally there is a function "testCacheMatrix" which does some tests.
## Use
## > source("cachematrix.R")
## > testCacheMatrix()
## to run these tests.

makeCacheMatrix <- function(x = matrix()) {
  # inv caches the inverse of the matrix;
  # No inverse is precomputed. Thus we set it to NULL
  inv <- NULL
  set <- function(y) {
    # Set new value for matrix
    x <<- y
    # When we set a new value, we have to reset the cached inverse
    inv <<- NULL
  }
  # We simply return the matrix
  get <- function() x
  # Set the inverse inv to the passed parameter
  setinverse <- function(inverse) inv <<- inverse
  # Return the cached inverse. If there is none cached
  # this will return NULL.
  getinverse <- function() inv
  
  list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## As the assignment tells (see
## https://class.coursera.org/rprog-006/human_grading/view/courses/972578/assessments/3/submissions
## for details):
## "cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve" will 
## "retrieve the inverse from the cache."
## 

cacheSolve <- function(x, ...) {
  # Get the cached inverse value
  inv <- x$getinverse()
  # Exists cached inverse? Great - we simply return it;
  # no further computation necessary.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # Otherwise we get the value of the matrix...
  data <- x$get()
  # ... compute its inverse ...
  inv <- solve(data, ...)
  # ... cache the value ...
  x$setinverse(inv)
  # .. and return the result of the inverse computation.
  inv
}

## Test code for makeCacheMatrix and cacheSolve.
## See comments in its code to see what is supposed
## to happen.
testCacheMatrix <- function() {
  # Use a Hilbert matrix for testing
  m <- matrix(c(
      1, 1/2, 1/3, 1/4,
    1/2, 1/3, 1/4, 1/5,
    1/3, 1/4, 1/5, 1/6,
    1/4, 1/5, 1/6, 1/7), 
    nrow = 4, ncol = 4)
  cm <- makeCacheMatrix(m)
  
  # No inverse is cached.
  # Thus this should print NULL
  print(cm$getinverse())
  
  inv <- cacheSolve(cm)
  print(inv)
  # Here we see that the Hilbert matrix has not nice numerical properties
  print(inv %*% cm$get())
  
  # Now we get the inv again
  inv <- cacheSolve(cm)
  # It should print the message 'getting cached data'.
  
  # See whether it's an inverse again:
  print(inv %*% cm$get())
  
  # We set a new value:
  cm$set(matrix(c(
      1, 1/2, 1/3, 
    1/2, 1/3, 1/4,
    1/3, 1/4, 1/5), 
    nrow = 3, ncol = 3))
  
  # The old cached inverse should be dropped.
  # Thus this should print NULL
  print(cm$getinverse())
}
