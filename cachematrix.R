## ----------------------------------------------------------------------------------------------------
## makeCacheMatrix: this function creates a "special" matrix object that can cache its inverse
##
## NOTE: Comments throughout the script below mirror the explanations provided in
## https://github.com/DanieleP/PA2-clarifying_instructions
## ----------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    # Changes the matrix stored in the main function
    set <- function(y)
    {
        x <<- y # Substitute the matrix x with y (the input) in makeCacheMatrix
        m <<- NULL # Restores to null the inverse of the matrix because the old inverse of the old matrix is not needed anymore
    }
    
    # Returns the matrix x stored in the main function. No input required
    get <- function() x
    
    # Store the inverse of the matrix in the variable m into makeCacheMatrix
    setinverse <- function(solve) m <<- solve
    
    # Return the inverse of the matrix
    getinverse <- function() m
    
    # Store the 4 function in the function makeCacheMatrix to make sure that when we assign makeCacheMatrix to an object, the object
    # has all 4 functions
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
    
}


## ----------------------------------------------------------------------------------------------------
## cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache

## NOTE: Comments throughout the script below mirror the explanations provided in
## https://github.com/DanieleP/PA2-clarifying_instructions
## ----------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    
    # cacheSolve first checks that the value m, stored in getinverse, exists and is not null
    m <- x$getinverse()
    
    # If it exists in memory, it returns the message "Getting cached data" and the value m
    if (!is.null(m))
    {
        message("Getting cached data")
        return(m)  # End the function in the case where m exists and is not null
    }
    
    # Get the matrix stored in makeCacheMatrix, m calculates the inverse and x$setinverse(m) stores it in the object 
    # generated with makeCacheMatrix
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
