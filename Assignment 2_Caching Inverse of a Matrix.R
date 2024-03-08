# Function to create a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse to NULL
  inv <- NULL  
  
  # Function to set the matrix and reset the inverse 
  set <- function(y) {
    x <<- y
    inv <<- NULL  
  }
  
  # Function to get the matrix 
  get <- function() x
  
  # Function to set the inverse 
  setinverse <- function(solve) inv <<- solve
  
  # Function to get the inverse 
  getinverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Function to compute the inverse of matrix object 
cacheSolve <- function(x, ...) {
  # Retrive the cached inverse 
  inv <- x$getinverse()
  
  # If the inverse is cached, return it 
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, compute it using solve 
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinverse(inv)
  
  # Return the computed inverse
  inv
}


