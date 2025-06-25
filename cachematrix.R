makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
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
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  
  if (nrow(mat) != ncol(mat)) {
    stop("Matrix is not square, cannot compute inverse.")
  }
  
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

find_inverse <- function(a) {
  my_matrix <- a
  cached_matrix <- makeCacheMatrix(my_matrix)
  inverse_matrix <- cacheSolve(cached_matrix)
  print(inverse_matrix)
}

