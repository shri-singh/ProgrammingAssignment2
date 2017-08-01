## CacheMatrix Assignment Submited by - Shri Singh
## This program allows to create a special matrix variable which has capability to store its inverse
### Code has two functions 1) makeCacheMatrix : which defines the matrix as list of functions
###                        2) cacheSolve      : which calculate the inverse if not cached and cashes otherwise returnsfrom cache


## makeCacheMatrix : returns a list of functions to-
### set matrix object
### get matrix object
### setsolve
### getsolve


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve : Function tries to retrieve the inversion of matrix from cache. If Matrix Inversion is called for first time
##               It calculates the inverse of matrix and saves in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
