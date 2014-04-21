## These two functions cache the inverse of a matrix.
## The purpose is to save the time involved in computing
## the inverse of a matrix every time it is needed.

## The makeCacheMatrix function creates a list consisting of
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
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


## The cacheSolve functions computes the inverse of the matrix
## stored in a makeCacheMatrix object. It first checks to
## see if the inverse has already been calculated and set in
## the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
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
