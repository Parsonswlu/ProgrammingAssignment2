## makeCacheMatrix and cacheSolve are functions designed to return the inverse of
## a matrix. If the matrix's inverse has already been computed, the function pulls it
## from the cache rather than re-computing the inverse.

## makeCacheMatrix takes in a matrix and returns a list of "set" and "get" functions 
## to be accessed later by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
        x<<- y
        inv <<- NULL
    }
    get <- function () x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve pulls the inverse of matrix x from the cache if it has already been
## computed, otherwise it computes the inverse of matrix x directly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message ("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}