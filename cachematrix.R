## These functions provide objects and an interface to calculate and
## store, for caching purposes, matrices and their inverses.

## makeCacheMatrix(matrix) instantiates a closure to store a matrix
## and, by request of the cacheSolve helper function, the inverse
## of that matrix

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL

  set <- function(newmat) {
    mat <<- newmat
    inv <<- NULL
  }
  
  get <- function() mat
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv

  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(makeCacheMatrix) solves and stores the inverse of a
## matrix in a makeCacheMatrix closure. If it is called while a valid
## cached inverse is already present it returns that rather than
## recalculating the inverse

cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()

  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}
