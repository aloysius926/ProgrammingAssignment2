## Extends the matrix class so you can save processing time if you are using the inverse
## of a matrix multiple times.  Rather than having to save the inverse the first time
## then look it up after that the functions take care of this functionality.
## Syntax: (x is a nonsingular matrix)
##     a <- makeCacheMatrix(x)
##     cacheSolve(a)   - actually calculates inverse
##     cacheSolve(a)   - uses the saved matrix inverse

## This function extends the matrix class in R to cache the inverse of that matrix
## It implements the getter and setter methods for this extension.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## Returns the inverse of a matrix from the cache if it is there.  Otherwise, it
## calculates it. This should really be a class method of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
