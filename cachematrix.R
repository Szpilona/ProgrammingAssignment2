## These functions calculate the matrix inverse and cache it for future uses.
## They handle only basic matrix inverse operations - there's no support for
## not invertible / singular matrices.

## This function creates data structure for matrix inverse supporting caching.
## It stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function gets the inverse of the given matrix (instantiating the object 
## makeCacheMatrix) from cache or calculates and stores it for future uses.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
