## This function cache's the inverse of a matrix for reducing 
## potentially time-consuming computations

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse<- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the makeCacheMatrix function.
## If the inverse has already been calculated for the same matrix, then 
## inverse will be retrieve from the cache

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)   
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
