makeCacheMatrix <- function (x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse. 
  ret <- NULL
  
  set <- function (y) {
    x <<- y
    ret <<- NULL
  }
  
  get <- function () x  
  
  setInverse <- function (inverse) {
    ret <<- inverse
  }
  
  getInverse <- function () ret
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function (x, ...) {
  # This function computes the inverse of the special "matrix" 
  # returned by makeCacheMatrix above. 
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve should retrieve the inverse from the cache.
  inverted <- x$getInverse()
  
  if(!is.null(inverted)) {
    message('getting cached data.')
    return (inverted)
  }
  
  data <- x$get()
  
  # Only can invert numeric / logical matrix
  # Integer matrix will not be converted.
  inverted <- solve(data) %*% data
  x$setInverse(inverted)
  inverted
}