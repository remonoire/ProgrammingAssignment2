## a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_M <- NULL
        ## Set Matrix
        set <- function(y) {
                x <<- y
                inv_M <<- NULL
        }
        ## Get Matrix
        get <- function() x
        
        ## Set Inverse
        setInv <- function(inv) inv_M <<- inv
        ## Get Inverse
        getInv <- function() inv_M
        ## List of Methods
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_M <- x$getInv()
        ## checks to see if the inverse has already been calculated
        if (!is.null(inv_M)) {
                message("getting cached data")
                return(inv_M)
        }
        ## data matrix
        data <- x$get()
        # Computing the inverse of a square matrix
        inv_M <- solve(data, ...)
        ## Sets inverse to object
        x$setInv(inv_M)
        ## Returns Matrix
        inv_M
        
}
