## Calculates the inversion of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # A cache holds the inversed matrix
    inversion <- NULL
    
    set <- function(y) {
        x <<- y
        inversion <<- NULL
    }
    
    get <- function() x
    
    setinversion <- function(inversed) inversion <<- inversed
    
    getinversion <- function() inversion
    
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Get the cached inversed matrix
    inversion <- x$getinversion()
    
    # Return the inversed matrix if it's not null
    if(!is.null(inversion)) {
        message("getting cached data")
        return(inversion)
    }
    
    # If there is no inversion cached, calculate it and cache it.
    data <- x$get()
    inversion <- solve(data)
    x$setinversion(inversion)
    inversion
}
