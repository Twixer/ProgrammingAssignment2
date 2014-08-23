## The following piece of code is composed of two functions : 
##   makeCacheMatrix(m=matrix()) : create an object for storing a matrix and its inverse
##   cacheSolve(x) : a function wich takes an object created by makeCacheMatrix and returns 
##                   the inverse of the matrix in makeCacheMatrix.
##
## Exemple of use : 
##    my.cached.matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
##    cacheSolve(my.cached.matrix)
## 
## If called a second time, the cached value of the inverse is returned.

makeCacheMatrix <- function(m = matrix()) {
    # This function create a matrix that is able to cache its inverse.
    #
    # Args : 
    #    m : an invertible matrix
    #
    # Returns : 
    #    a list of available functions
    
    cache.inverse <- NULL
    
    # This function set a new invertible matrix
    # So it will also erase the previous cache value.
    set <- function(y) {
        m <<- y
        cache.inverse <<- NULL
    }
    
    # Function to get back the invertible matrix stored in this function
    get <- function() m
    
    # Function to set the cached value of the inverse of the matrix (m)
    setsolve <- function(solve) cache.inverse <<- solve
    
    # Function to get back the cached value of the inverse of the matrix (m)
    getsolve <- function() cache.inverse
    
    # The returned value : a list of available functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
    # This function takes an object created with makeCacheMatrix an use it to return
    # the inverse of the matrix in makeCacheMatrix.
    #
    # Args : 
    #    x : a makeCacheMatrix object
    #
    # Returns : 
    #    the inverse of the matrix in mamakeCacheMatrix. The value is either calculated
    #    (the first time) or the cache value.
    
    s <- x$getsolve()
    
    # There is already a cached value for the inverse
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # First calculation of the inverse
    data <- x$get()
    s <- solve(data, ...)
    # Setting the cache value
    x$setsolve(s)
    
    # the inverse is returned
    s
}


