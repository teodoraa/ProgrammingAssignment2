## Because matrix operations are time-consuming, the following 
## functions cache the inverse of a matrix, so that if needed
## it can be obtained without having to be recomputed.

## This function creates a list of 4 functions to set and get a 
## matrix and to set and get the inverse of the matrix. The 
## parameter it takes is a square matrix. The matrix is assumed
## to be invertable.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of a matrix if the inverse
## has been computed previously and the matrix has not changed. 
## Otherwise, it computes the inverse and returns it. The 
## parameter is a list created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
