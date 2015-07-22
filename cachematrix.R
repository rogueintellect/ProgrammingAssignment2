## makeCacheMatrix creates a list containing functions that
## set the value of the matrix;
## get the value of the matrix;
## set the value of the inverse of the matrix, using solve;
## and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) { 
    ## Default is set to a null matrix.
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix cached
## through makeCacheMatrix. If the inverse for the 
## matrix has already been calculated and cached,
## the cached value is returned.

cacheSolve <- function(x,...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
