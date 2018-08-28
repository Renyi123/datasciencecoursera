## makeCacheMatrix:
## Catching the inverse of a matrix
## It is time consuming to compute matrix inversion
## Caching the inverse of a matris rather than compute it everytime
## saves time
## The functions below are used to create a object to store a
## matrix and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse 
      getinv <- function() inv
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)

}


## This function computes the inverse of the matix created
## by makeCachematrix above.
## If the incerse is in the cache, it would just retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}

