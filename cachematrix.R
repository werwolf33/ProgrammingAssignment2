# These functions perform the following:
#
# makeCacheMatrix - creates a special "matrix" object that can cache its 
#                  inverse.
#
# cacheSolve - computes the inverse of the special "matrix" returned  
#              by makeCacheMatrix above. If the inverse has already 
#              been calculated (and the matrix has not changed), 
#              then the cachesolve should retrieve the inverse from the cache.

#

# This function creates creates a list containing a function to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of inverse of the matrix
# - get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matinv <<- inverse
    getinverse <- function() matinv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    matinv <- x$getinverse()
    if(!is.null(matinv)) {
        message("getting cached data.")
        return(matinv)
    }
    data <- x$get()
    matinv <- solve(data)
    x$setinverse(matinv)
    matinv
}
