## Implements functions for cached computation of matrix inverse for a given input matrix 'x'

## get and/or set the values of the cached input 'x' and the cached inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
        ## get and set functions for the input matrix 'x'
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        
        ## get and set functions for the inverse of the input matrix
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
