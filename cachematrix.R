# R Programming Assignment2 Comments:
# Modify the two function templates provided by instructor R. Peng, in the
# assitnment to store a matrix and cache it's inverse.
#
# Function "makeCacheMatrix" creates a special "vector" - a list - that
# contains four functions to:
# 1) "set" - set the value of the matrix
# 2) "get" - get the value of the matrix
# 3) "setinverse" - set the value of the inverse
# 4) "getinverse" the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function "cacheSolve" calculates and returns the inverse of the object created
## with the "makeCacheMatrix" function which first checks to see if the inverse
## of the matrix has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse of the matrix in the cache
## via the "setinverse" function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  ## Skip calculatuion and return the cached inverse of matrix 'm'.
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m          ## Calculate, cache, and return the inverse of matrix 'm'.
}
