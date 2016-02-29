## Calculating the inverse of the matrix is potentially costly operation when performed repeatedly.
## The set of functions below cache and retrieve previously calculated inverses of the matrices

## This function creates a cache-aware implementation of matrix 
## that caches its own inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## reset the source matrix associated with this cached matrix object
  set <- function(y) {
    x <<- y
    ## upon resetting the source matrix, the cache is cleared
    m <<- NULL
  }
  ## returns the original matrix
  get <- function() x
  
  ## persist the inverse of the matrix in the cache
  setinverse <- function(matrix_inverse) m <<- matrix_inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the matrix by
## 1. First looking in the cache
## 2. Calculating it if it is not in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
