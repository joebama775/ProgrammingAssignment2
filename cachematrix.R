# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute or retrieve the cached inverse of the matrix
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

#takes an input matrix, runs the prior functions, and prints the inverse
#used this for testing
find_inverse <- function(a) {
  my_matrix <- a
  cached_matrix <- makeCacheMatrix(my_matrix)
  inverse_matrix <- cacheSolve(cached_matrix)
  print(inverse_matrix)
}

