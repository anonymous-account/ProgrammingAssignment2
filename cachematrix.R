
## This function creates a special "matrix" object that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # the cached inverse of the matrix (originally NULL)
  inv <- NULL
  
  # This function sets/changes the matrix value (and, subsequently,
  # resets any cached inverse value to NULL) IF the new matrix/value is 
  # different from the current value (if the two matrices are the same,
  # then we don't need to re-calculate the cached value, if any, since it
  # would be the same)
  set <- function(y) {
    if (!identical(x,y)) {
      x <<- y
      inv <<- NULL
    }
  }
  
  # This function gets the current matrix value
  get <- function() x
  
  # This function sets the inverse for this matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # This function gets the current inverse of this matrix
  getInverse <- function() inv
  
  ## Return a special "matrix" object (a list of the above customized
  ## functions, which all relate to the given matrix, x)
  list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object returned by
## the makeCacheMatrix() function, above.  If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve() function
## retrieves the inverse from its cache.
cacheSolve <- function(x, ...) {  
  ## Check if we've already calculated & stored/cached the inverse. If so, return
  ## that cached value.
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## We haven't already calculated the inverse for this matrix yet. So...
  ## calculate the inverse via the solve() method, store/cache that value, and
  ## return it.
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
