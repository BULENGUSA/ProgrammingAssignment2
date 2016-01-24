## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. What follows are two functions to do exactly that.

## The first function "MakeCacheMatrix" creates a special "matrix", which is a 
## list containing a function to do the following:
## 1. Set the value of a matrix.
## 2. Get the value of a matrix.
## 3. Set the value of the inverse of that matrix.
## 4. Get the value of the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The second function returns the inverse of the matrix. First, it checks if 
## the inverse has already been calculated and cached. If it has, it returns the 
## inverse matrix from the cache and skips the calculation. If is hasn't, it 
## calculates the inverse of the matrix, sets it in the cache, and returns the 
## inverse of the matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data.")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}