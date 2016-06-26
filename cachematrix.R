makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(data) {
    x <<- data
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  
  data_inverse <- x$getInverse()
  if (!is.null(data_inverse)) {
    message("getting cached data")
    return(data_inverse)
  }
  mat <- x$get()
  data_inverse <- solve(mat, ...)
  x$setInverse(data_inverse)
  data_inverse
}
