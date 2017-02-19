## cachematrix has two funtions.
##
## The first makeCacheMatrix creates a listing that has functions to:
##  1. set the value of the matrix,
##  2. get the value of the matrix,
##  3. set the value of the inverse,
##  4. get the value of the inverse.
## It uses the function solve.
## 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULLsource
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the value of the inverse matrix, the matrix
## which was the argument of makeCacheMatrix.

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
