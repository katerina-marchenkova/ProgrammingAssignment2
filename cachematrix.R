## Pair of functions that cache the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function()
    {
        x
    }
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    getInverse <- function()
    {
        inv
    }
    setInverse <- function(inverse)
    {
        inv <<- inverse
    }
    list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    ## Returns a matrix that is the inverse of 'x'
    inv
}
