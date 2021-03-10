## Coursera, R Programming, March 2021
## Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   # Cached value of inverse, if exists
  
  set <- function(y) {
    i <<- NULL  # Invalidate cached inverse
    x <<- y     # Save matrix to this functions environment
  }
  
  get <- function() x  # Return matrix
  
  getinverse <- function() i # Return cached inverse, if any
  
  setinverse <- function(inverse) i <<- inverse # Cache the inverse
  
  # Return list of functions for working with underlying matrix and its inverse
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


# CacheSolve will return the inverse of matrix contained within its
# first argument.  Upon first calculation, the 'solve' function is called to
# calculate the returned inverse and cache it; subsequent calls return the cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    # If cached inverse exists, fetch and return it
    message('Fetched cached inverse!')
    i
  }
  
  # Else calculate inverse using underlying data, cache and return
  data <- x$get()          # Extract the underlying data
  inv <- solve(data, ...)  # Calculate its inverse
  x$setinverse(inv)        # Cache the inverse for re-use
  inv                      # Return the inverse
}
