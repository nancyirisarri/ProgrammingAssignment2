## Return cached inverse of a matrix or, if not cached,
## compute if.

## Create a matrix object and cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    mat <<- y
    inverse <<- NULL
  }
  
  get <- function() mat
  
  setinverse <- function(solve) inverse <<- solve
  
  getinverse <- function() inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Get from cache or calculate the inverse of the matrix.

cacheSolve <- function(mat, ...) {
  
  # Get from cache and return if not null.
  inverse <- mat$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Calculate the inverse using solve().
  data <- mat$get()
  
  inverse <- solve(data, ...)
  
  mat$setinverse(inverse)
  
  inverse
}
