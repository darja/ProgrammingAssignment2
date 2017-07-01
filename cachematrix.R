## Returns a list of functions to access matrix itself and inverted one
## get -- returns a matrix
## set -- sets a matrix and clears cached inverted matrix
## getInv -- returns cached inverted matrix
## setInv -- initializes inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) xinv <<- inv
    getInv <- function() xinv
    
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Returns cached inverted matrix, if it exists, or computes it
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setInv(inv)
    inv
}
