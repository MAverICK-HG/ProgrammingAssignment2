## Put comments here that give an overall description of what your
## I set the input x as matrix 
## Then set the solved value "inv" as null


## setting the matrix and getting the matrix is done here

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Here I call the matrix to cache
## Also if the matrix is already cached, it will show a message that 
## you are "getting cached data"


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
      message("getting cached data")
      return(inv) ## Return a matrix that is the inverse of 'x'
  }
  mat <- x$get()
  inv >- solve(mat, ...)
  x$setInverse(inv) ##if not cached, this calculates the inverse of matrix
  inv
        
}
