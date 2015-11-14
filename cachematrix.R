## Put comments here that give an overall description of what your
## functions do
# Two small functions to cache matrices and inverse.
# Example of use:
# First step: Create a (square) matrix that is invertible:
# For instance A <- matrix(c(2,0,0,0,-1,0,0,0,1),3)
# Second step: Create the structure:
# bb <- makeCacheMatrix(A)
# Then, to get the matrix A, type: bb$get()
# To compute the inverse of matrix A: cacheSolve(bb)
# (during 1st call, it computes it, other calls it recalls from cache)
# to see the inverse matrix (after being computed): bb$getInverse()

## Write a short comment describing this function
# Create a structure with 3 elements: get (to get the original vector), 
# setInverse (that set the value of the inverse of the matrix) and 
# getInverse to recover it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <-function() x
  setInverse <- function(val) inv <<- val
  getInverse <- function() inv 
  list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# Test if the inverse matrix is in the cache, by checking the 
# value of getInverse. If the value has been computed yet, 
# the functions returns the value of getInverse. Otherwise
# (i.e. if getInverse returns NULL), then it computes the inverse
# of the matrix and stores it in the structure using function 
# setInverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getInverse()
  if (!is.null(inv)) {
    message ("getting from cache")
    return(inv)
  }
  inv <-solve(x$get())
  x$setInverse(inv)
  inv
}
