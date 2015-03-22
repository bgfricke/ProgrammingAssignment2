## The makeCacheMatrix and cacheSolve functions work together to form a system 
## that supports a matrix type with a cacheable inverse. The makeCacheMatrix
## function provides the list of functions that make it possible for cacheSolve
## to store and look for a cache of the inverse of the matrix contained within
## the makeCacheMatrix environment.


## Function that acts like a class constructor in that it intializes an
## internally held matrix. The return value is a list of functions that behave
## like class methods for getting and setting the internally held matrix and
## cached inverse of internally held matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        # Note that this type of assignment makes the parent's environment
        # accessible to this inner funtion's environment.
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of a matrix. Internally, operates on the list of
## functions returned from makeCacheMatrix. So, effectively the function
## operates on a cacheable inverse matrix "object" (i.e., the environment that
## contains a matrix, cached inverse, and functions for getting and setting the
## matrix and its inverse). If the inverse isn't presently cached, it is
## computed, stored inside the matrix object, and returned. If the inverse is
## cached, it is retrieved from the matrix object, a notfication is printed, and
## the inverse is returned. It is assumed that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## Tests to ensure that an inverse is computed, cached and retrieved.
## Additionally, tests to ensure that the cache is reset when the matrix is
## reset.

testCacheSolveWorks <- function(){
    x <- stats::rnorm(16)
    dim(x) <- c(4,4)
    y <- makeCacheMatrix(x)
    
    identityMatrix <- cacheSolve(y) %*% x
    test1 <- all.equal(identityMatrix, diag(4))
    identityMatrix <- cacheSolve(y) %*% x
    test2 <- all.equal(identityMatrix, diag(4))
    
    x <- stats::rnorm(16)
    dim(x) <- c(4,4)
    y$set(x)
    identityMatrix <- cacheSolve(y) %*% x
    test3 <- all.equal(identityMatrix, diag(4))
    identityMatrix <- cacheSolve(y) %*% x
    test4 <- all.equal(identityMatrix, diag(4))
    test1 & test2 & test3 & test4
}