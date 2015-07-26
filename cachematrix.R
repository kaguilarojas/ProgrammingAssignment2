## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve:  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##              above. If the inverse has already been calculated (and the matrix has not changed), 
##              then the cachesolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## These functions are based on the Assignment Example: Caching the Mean of a Vector

## The first function creates a special "matrix" which is a list containing functions to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## setting the initial value as NULL
        set <- function(y) {   ## Assign the value to x and set to NULL when set is called
                x <<- y
                m <<- NULL
        }
        get <- function() x    ## Get value when available
        setinverse <- function(inv) m <<- inv  ## Set inverse value when calculated
        getinverse <- function() m   ## Get inverse value when available
        list(
                set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}

## This function calculates the inverse of the special "matrix" created with the above function.
## It will check first to see if the inverse has already been calculated (!is.null(m)). 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the mean in the cache via the setinverse function.
## reusing cached result if it is available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()   ## Check existance of inverse value
        if(!is.null(m)) {     ## If value available - not NULL - returns it
                message("getting cached data")
                return(m)
        }
        data <- x$get()   
        m <- solve(data, ...)  ## Use solve(x) to calculate inverse
        x$setinverse(m)
        m
}
