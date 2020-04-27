## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL #<<- indicates that we are assigning the values in the global environment
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
#computes the inverse of the matrix returned by makeCacheMatrix above,
  #if the inverse is not present in the cache memory.
#If the inverse matrix already exists in the cache memory then,
  #it will fetch the matrix from cache instead of computing again.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) { #checking whether inverse matrix is already present
            message("getting cached matix inverse")
            return(inv) #if yes then returning the inverse matrix without computing again
      }
      mdata <- x$get() #getting the matrix from the makeCacheMarix function
      inv <- solve(mdata, ...) #computing the inverse if is not present in cache memory
      x$setinverse(inv)
      inv
}
