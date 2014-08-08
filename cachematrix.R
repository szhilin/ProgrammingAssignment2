## "Smart" matrix that caches its inverse
##
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: computes the inverse or gets its cached copy for the special "matrix" 
##            constructed makeCacheMatrix.
##            NB! No checks if the input matrix is invertable.


## Creates a special "matrix" object that caches its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xinv <<- inv
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Computes inverse matrix for special "matrix" x if this inverse marix differs from the cached copy

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## The end