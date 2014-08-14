## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: a list of functions that allow to store and return
## the values of a matrix M and its inverse invM

makeCacheMatrix <- function(M = matrix()) {
    # this is the inverse matrix from the cache
    invM <- NULL
    
    # this function sets the matrix values and resets
    # cache to NULL
    set <- function(y) {
        M <<- y
        invM <<- NULL
    }

    # this function returns the matrix
    get <- function() M

    # this function copies the inverse matrix z
    # into the cached element invM
    setInv <- function(z) invM <<- z

    # this function returns the inverse matrix
    getInv <- function() invM

    # defining the list of functions
    list(set = set, get = get,
        setInv = setInv, getInv = getInv)
}


## cacheSolve: a function that returns the inverse of a matrix. Using the
## functions given in makeCacheMatrix, it can get a value stored in the
## cache, rather than calculating the inverse matrix again.

cacheSolve <- function(x, ...) {
    # request the value of the inverse matrix
    invM <- x$getInv()

    # if the result is not in the cache,
    # the inverse matrix will first be generated,
    # and then it is copied into the cacheMatrix
    # for future use
    if(is.null(invM)){
        M <- x$get()  # get the data
        invM <- solve(M, ...) # invert the matrix
        x$setInv(invM) # save in the cacheMatrix
    }

    # return the inverse matrix value
    invM
}
