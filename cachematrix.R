## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Compute the inverse of the input matrix 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        ## Retrieve the cached matrix inverse
        m <- x$getinverse()
        ## Check if the cached matrix inverse is NULL, which means no cached matrix exists
        if(!is.null(m)) {
          ## If inverse is cached retrieve it and return it
          message("getting cached data")
          return(m)
        }
        ## inverse is not cached, so retrieve the input 'x'
        data <- x$get()
        ## compute the inverse of 'x'
        m <- solve(data, ...)
        ## update the cache with the computed matrix inverse of 'x'
        x$setinverse(m)
        ## Return the computed inverse of 'x'
        m
}
