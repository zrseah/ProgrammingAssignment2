makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Create a special "matrix" object
m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute the inverse using cacheSolve
cacheSolve(m)
# This will compute the inverse and cache it

# Retrieve the inverse from the cache
cacheSolve(m)
# This will retrieve the cached inverse without recomputation
