##  Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## set the value of the matrix, get the value of the vector, set the value of the mean, get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function () inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
