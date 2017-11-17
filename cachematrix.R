## Below pair of functions can be used for finding the inverse of a matrix
## and Cache the result to avoid redundant computation

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(inv_matrix) {
    m <<- inv_matrix
  }
  getInvMatrix <- function() m
  list(set=set,get=get,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  mtrx <- x$get()
  m <- solve(mtrx)
  x$setInvMatrix(m)
  m
}
