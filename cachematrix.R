## The code returns the inverse of a matrix from cache, if already present else calculates the inverse.
## Thus reducing the recalculating efforts.
## This is achieved through two functions: makeCacheMatrix() and cacheSolve()

## makeCacheMatrix() is the parent function which has four other functions in its environment: 
## setmatrix(),getmatrix(),setinverse(),getinverse().
## Setmatrix() - sets the matrix value, x
## getmatrix() - accesses the matrix value, x
## setinverse() - sets the inverse of the matrix, m
## getinverse() - accesses the inverse of the matrix,m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve() returns the inverse of the matrix. It checks for the inverse in cache (if already calculateded)
## else calculates the inverse of the matrix and sets the value to cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
