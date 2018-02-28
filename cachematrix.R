## This file creates two functions: makeCacheMatrix and cacheSolve. The purpose of these functions is to
## store values of potentially time-consuming computations (such and matrix inverses calculations) and
## retrieve the stored values instead of re-calculating inverses unnecessarily.

## makeCacheMatrix takes a matrix input and returns four functions: set, setinv, get, and getinv

makeCacheMatrix <- function(x = matrix()) {  # x matrix is initialized to NULL in the arguments field to avoid an error
    inv <- NULL     # create and initialize inv variable to store the matrix inverse
    
    set <- function(y) {
        x <<- y   # store new matrix y into parent environment
        inv <<- NULL  # reset previously calculated inverse to NULL in anticipation of new inverse calculation. This NULL
                      # value becomes a flag in the cacheSolve function
    }
        
    get <- function() x    # retrieve stored matrix x from the parent environment
    
    setinv <- function(inverse) inv <<- inverse    # the new matrix inverse is sent to setinv, and setinv stores it in parent env.
    
    getinv <- function() inv    # getinv() retrieves the cached value of the inverse of the current matrix
        
    list(set = set, get = get,                  
         setinv = setinv, getinv = getinv)    # a list of named variables (functions) is returned so that the functions
                                                # are available in the parent environment

}


## cacheSolve(x, ...) accepts the current matrix stored in x, uses getinv() to determine if the matrix has changed since the 
## inverse was last calculated. If not, the cached inverse is returned. If the matrix has changed, the new inverse is 
## calculated and then returned.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()   # retrieve the last calculated inverse. If matrix has changed, inv = NULL from set() function
    if (!is.null(inv)) {
        message("getting cached data")   # if inv is not NULL, matrix has not changed. Retrieve cached inverse and return.
      return(inv)
    }
    
    data <- x$get()            # else, if inv is NULL, the matrix has been changed. 
    inv <- solve(data, ...)            # Inverse of the new matrix must be calculated
    x$setinv(inv)                       # and the new inverse must be cached to parent envir using setinv() 
    inv                         # return the inverse calculated

}
