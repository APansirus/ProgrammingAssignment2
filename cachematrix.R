## These functions cache a matrix and compute and cache its inverse

## This function creates a special "matrix" object that can cache itself and 
## its inverse. 
## If I understand the subfunctions taken from the vector example correctly:

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # set assigns its argument to the argument of the parent function and sets 
    # the variable for the inverse back to NULL (it deletes the old inverse)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get returns the argument of the parent function
    get <- function() x
    
    # setInverse caches its argument to the inv-variable from the parent function
    setInverse <- function(inverseM) inv <<- inverseM
    
    # getInverse returns the inv-variable from the parent function
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated for this 
## matrix, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    # if there's no value for the inverse cached...
    if(!is.null(inv)) {
        message("retrieving the inverse from the cache")
        return(inv)
    }
    
    # ... cacheSolve gets the matrix from makeCacheMatrix...
    data <- x$get()
    
    # ... computes its inverse ...
    inv <- solve(data, ...)
    
    # ... caches the inverse
    x$setInverse(inv)
    
    # and then invisibly returns the result
    invisible(inv)
}
