## These functions will calculate the inverse of X and store it
## If the inverse is called on a function that already has had its
## inverse computed, the inverse will be read from the cache to save
## computation time


## Creates a special matrix that contains important functions
## such as getting and setting the values of the matrix
## and getting and setting the inverse of the matrix

makeCacheMatrix <- function(x = matrix) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix, but first checks
## if the inverse has already been computed to avoid 
## recalculating it

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
