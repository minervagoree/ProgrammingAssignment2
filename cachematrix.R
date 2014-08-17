## Author: Minerva Goree 8/17/2014
## Coursera: R Programming taught by John Hopkins University
## These functions are able to cache potentially time-consuming computations for an inverse of a
## matrix.

## The makeCacheMatrix function creates a special object that computes or caches the inverse of
## a matrix.  

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get<- function() {x}
        setInverse <- function(solve) {m <<- solve}
        getInverse <- function() {m}
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function computes the inverse of a matrix that is returned by makeCacheMatrix().
## If the inverse has been calculated and stored, plus the matrix is the same one, then  cacheSolve
## will not recalculate, but retrieve the saved solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
