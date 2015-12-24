## Caching The Inverse of a Square Matrix:
## The objective of the following function is to cache the 
## inverse of a square matrix instead of computing it repeatedly.
## To this end, two functions are presented: create a matrix
## which the inverse can be cached, and compute the inverse of 
## a matrix in a way that if the inverse has been calculate, it
## retrives the inverse from cache.

## Creating the special MATRIX object, which the inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
        
        invse <- NULL
        
        set <- function(y){
                x <<- y
                invse <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) invse <<- inverse
        getinverse <- function() invse
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Computing the inverse of a matrix such a way that if the inverse has been
## calculated, the function retrive the inverse from cache.

cacheSolve <- function(x, ...) {
        
        invse <- x$getinverse()
        
        if (!is.null(invse)) {
                message("Getting Cached Data ...")
                return(invse)
        }
        
        m <- x$get()
        invse <- solve(m, ...)
        x$setinverse(invse)
        
        invse
}
