## Creating a s pair of functions that cache the inverse of a matrix

## Creating a special matrix object where the inverse can be cached 

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## Method to get the matrix
  get <- function() {
    m
  }
  ## Method to set and get the inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    i
  }
  ## Return a list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## A function to retrieve cached inverse if inverse has alread been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If already inversed, return m 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object and calculating inverse
  data <- x$get()
  m <- solve(data)
  
  ## Set the inverse to the object
  x$setInverse(m)
  m
}
