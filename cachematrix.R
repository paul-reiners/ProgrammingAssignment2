## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. We define a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) xInverse <<- mean
        getinverse <- function() xInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated (and the 
## matrix has not changed), then `cacheSolve` retrieves the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInverse <- x$getinverse()
        if(!is.null(xInverse)) {
                message("getting cached data")
                return(xInverse)
        }
        data <- x$get()
        xInverse <- solve(data, ...)
        x$setinverse(xInverse)
        xInverse
}
