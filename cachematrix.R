##make cache matrix creates the list of functions that allow the matrix to cache


## This function creates the list of functions that allows for caching the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function()x
  setinverse <- function(calculated)inverse <<- calculated
  getinverse <- function()inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function uses the list of functions from makeCacheMatrix to find the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse))
  {
    message("Getting cached matrix")
    return(inverse)
  }
  data <- x$get
  inverse <- solve(data(),...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
