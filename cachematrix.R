## Script to make a special matrix with Inverse saved if already solved and
## special solver for inverse matrix of special matrix

## Author: Duc Nguyen. Date: Dec 2023

## Function to make special matrix. Create matrix and set functions to save
## Inverse matrix information if solved

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) I <<- Inv
  getInv <- function() I
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Function to solve for Inverse matrix of a special matrix
## Only solve if needed, if info already saved, used cached data

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data,...)
  x$setInv(I)
  I
}
