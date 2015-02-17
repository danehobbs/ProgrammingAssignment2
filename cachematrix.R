## These functions are to be used jointly. Matrix inversion is usually a costly
## computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly. 
## 
## Example: 
##      given a matrix X, invoke the functions by storing the output of
##      makeCacheMatrix(x) to a temporary "list"
##      x1 <- makeCacheMatrix (x)
##      Then when invoking the command:
##      cacheSolve(x1)
##      it will return the inverse of x
##      It it hasn't already run before, it will solve for the inverse of x
##      If it has already run before, it will return the cached value of inverse
##       of x.

## The makeCacheMatrix function creates a
## family of functions to:
##      1) set the value of the matrix  ($set)
##      2) get the value of the matrix  ($get)
##      3) set the value of inverse of the matrix ($setsolve)
##      4) get the value of inverse of the matrix ($getsolve)

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) minv <<- solve
        getsolve <- function() minv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## cacheSolve calculates the inverse of a matrix. If the matrix has been stored
## in the cache and inverse has already been calculated it will pull the result
## from the cache.  Otherwise it will recompute the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getsolve()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setsolve(minv)
        minv
        
}