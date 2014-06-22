## Implement a matrix computation caching framework
## that holds cached computed values for a matrix 
## (such as the matrix inverse) inside the cache 
## strucutre, so that subsequent calls for the inverse 
## will return the cached value, if available.


makeCacheMatrix <- function(x = matrix()) {
    # Creates a cacheable matrix inverter
    # 
    # Args:
    #  x: A standard R matrix 
    #
    # Returns:
    #  An object that can be used with the cacheSolve function
    #  supporting cached matrix inversion
    
    
    xInv <- NULL
    set <- function(y) 
    {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) xInv <<- inverse
    getInverse <- function() xInv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Compute or retrieve the inverse of the cached matrix 
    # Retrieves a cached value of the inverse, or calculates and 
    # stores if necessary.
    # 
    # Args: 
    #  x: A cacheable matrix created by makeCacheMatrix
    #
    # Returns: 
    #  The inverse of the matrix in x

    xInv <- x$getInverse()
    if (!is.null(xInv))
    {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInverse(xInv)
    xInv
}
