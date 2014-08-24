## A pair of functions that cache the inverse of a matrix to avoid resource 
## consumption caused by matrix inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        i <- NULL
        
        ## Method to set the matrix
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        ## Get  matrix
        get <- function() {
                ## Return result
                m
        }
        ## Set the matrix inverse  
        setInverse <- function(inverse) {
                i <<- inverse
        }
        ## Get the matrix inverse
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        ## Return list methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## the prior makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix as inverse of 'x'
        m <- x$getInverse()

        ## Return  inverse if its already set
        if( !is.null(m) ) {
                message("Retrieving cached data")
                return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set inverse to object
        x$setInverse(m)
        
        ## Return matrix
        m
}
