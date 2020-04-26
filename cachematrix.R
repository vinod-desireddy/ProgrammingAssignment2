## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
#computes the inverse of the matrix returned by makeCacheMatrix above if it is not present in the cache already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      mdata <- x$get()
      inv <- solve(mdata, ...)
      x$setinverse(inv)
      inv
}
