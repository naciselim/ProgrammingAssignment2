## The following two functions will create a special matrix object that caches its inverse.
## This way the inverse of the matrix is calculated once and stored in this special object and
## when it is requested again, the stored inverse matrix is returned instead of re-calculating
## the possibly expensive inverse operation.
##
## Usage:
## m <- makeCacheMatrix()   # creates empty special matrix object
## m$set(mtrx)              # sets the value of matrix as mtrx (must be square and invertable)
## cacheSolve(m)            # retuns and sets inverse of matrix stored in m
## m$get %*% m$getInvers()  # this should return identity matrix
##

## Function makeCecheMatrix creates the special matrix object from the given matrix.
## Input parameters: None.
## Return:    
##    a list containing references to four functions
##    1) set: set the value of matrix
##    2) get: get the value of matrix
##    3) setInverse: set the value of inverse of matrix
##    4) getInverse: get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv_matrix) inv <<- inv_matrix
    getInverse <- function() inv
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Function cacheSolve simply calculates and returns the inverse of a matrix given as
## the special matrix object created by makeCacheMatrix object.
## Input parameters:
##      x: Special matrix object created with makeCacheMatrix and set with its set function.
## Returns:
##      inverse of matrix x.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    squarematrix <- x$get()
    inv <- solve(squarematrix, ...)
    x$setInverse(inv)
    inv
}
