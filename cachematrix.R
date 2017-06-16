## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly.


# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(inupt = matrix()) {
  invM <- NULL
  setM <- function(x) {
    input <<- x
    invM <<- NULL
  }
  getM <- function() input
  setInvM <- function(inverse) invM <<- inverse
  getInvM <- function() invM
  list(setM=setM, 
       getM=getM,  
       setInvM=setInvM, 
       getInvM=getInvM 
  )
}


## cacheSolve takes the output from makeCacheMatrix() as an input
## and returns the inverse of the original matrix
cacheSolve <- function(makeCacheMatrixOutput, ...) {
  invM = makeCacheMatrixOutput$getInvM()
  
  # Is the inverse is in memory?
  if (!is.null(invM)){
    message("Using cache!")
    return(invM)
  }
  
  else {
    message("Placing into cache!")
    mCM = makeCacheMatrixOutput$getM()
    invM = solve(mCM, ...)
    makeCacheMatrixOutput$setInvM(invM)
    return(invM)
  }
  
}