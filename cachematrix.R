## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# makeCacheMatrix accepts a matrix, then initializes and returns an
#   environment accessed by the cachematrix function
# variables are stored in the 'makeCacheMatrix' environment and can be
#   accessed and manipulated by both functions

makeCacheMatrix <- function(x = matrix()) {
    # variables:
    # x: initial matrix passed to function to be inverted
    # im: inverted matrix
    im <- NULL

    # set new matrix in x without calling makeCacheMatrix()
    set <- function(y) {
        x <<- y
        im <<- NULL
    }

    # retrieve value of x
    get <- function() {
        x
    }

    # save inverse of matrix x in cache
    setInverse <- function(invMat) {
        im <<- invMat
    }

    # retrieve inverse matrix from cache
    getInverse <- function() {
        im
    }

    # return getters & setters
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
#
# cacheSolve accepts a "makeCacheMatrix" environment and checks for
#   a cached inverse for matrix x.
# if cached result is found, it is returned
# if cached result is not found, it is calculated, stored in the cache
#   and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # fetch inverse matrix from cache (if available)
    im <- x$getInverse()

    # if im is NULL, no cached data found
    if(is.null(im)) {
        # no inverse matrix found, so calculate it
        message("no cached data, calculating inverse")
        # get inverse of matrix x
        im <- solve(x$get())
    } else {
        # found an inverse matrix, so return it
        message("retrieving cached data")
    }

    # save inverse matrix in cache
    x$setInverse(im)

    # return inverse matrix
    im
}
