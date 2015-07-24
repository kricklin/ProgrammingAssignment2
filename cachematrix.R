## R Programming Assignment2 Comments:
##
## Author: kricklin
##
## Last Updated 07/24/2015
##
## Modify the two function templates provided by instructor R. Peng, in the
## assignment description to store a matrix and cache it's inverse.
##
## Function "makeCacheMatrix" creates a list vector of functions that
## are called to process a matrix:
## 1) "set" - stores the matrix to be inverted - used by makeCacheMatrix()
## 2) "get" - retrieves the stored matrix - used by cacheSolve()
## 3) "setinverse" - stores the inverse of the matrix - used by cacheSolve()
## 4) "getinverse" - retrieves the inverted matrix - used by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ## Only "set" is used by "makeCacheMatrix".
                x <<- y      ## Store the matrix in a differnt environment.
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,   ## Store the functions in a list.
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function "cacheSolve" calculates and returns the inverse of the matrix stored
## by "makeCacheMatrix" function. If the matrix inverse has already been
## calculated the cached inverted matrix is retrieved and returned. If the
## inverted matrix is not already cached, the matrix inverse is
## calculated, cached, and returned.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  ## Skip calculatuion and return the cached inverse of matrix 'm'.
        }
        data <- x$get()
        m <- solve(data, ...) ## solve() calculates the matrix inverse, saving us the trouble.
        x$setinverse(m)
        m          ## Calculate, cache, and return the inverse of matrix 'm'.
}
