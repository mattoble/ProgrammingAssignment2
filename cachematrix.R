## Overall: These two functions work together to calculate the inverse of a matrix
## and store (cache) the result. This way, if the inverse is needed again
## and the matrix hasn't changed, the stored result is used, saving computation time.


## makeCacheMatrix: This function creates a special "matrix" object that can:
## 1. Store the actual matrix.
## 2. Store the calculated inverse of that matrix.
## It's like creating a special box that holds the matrix and a spot for its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    i
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: This function calculates the inverse of the special "matrix"
## created by makeCacheMatrix.
## If the inverse has already been calculated and stored, it quickly grabs
## that stored version. Otherwise, it does the calculation, stores it
## for next time, and then gives the result.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  
  if( !is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  m
}
