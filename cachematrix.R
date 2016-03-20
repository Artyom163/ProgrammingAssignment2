## These functions are used to create a special object 
## that stores a matrix and caches its inverse.

## This function creates a special "matrix" object
## that can cache its inverse. It returns
## a list of functions that can set and get the values
## of both matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(x) inv <<- solve(x)
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix. 
## If it has already been calculated, the function returns
## the cached value of the inverse. Otherwise, the function 
## calculates it and stores in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}