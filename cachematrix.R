## This function caches a time-consuming computation.
## It contains two parts: the first part creates a matrix, 
## including the caching option; the second part computes 
## the inverse of the matrix while checking the cache.


## Creates matrix as a 'special list' with 4 elements
## (set, get, setInverse, getInverse).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function() {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function () m
        list( set = set, get = get,
              setInverse = setInverse, getInverse = getInverse)
}

## Computes inverse of matrix only if it isn't stored in cache m.
cacheSolve <- function(x, ...){
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