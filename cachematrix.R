## Author: RAL 
## Date: 2014-06-18
## Title: Programming Assignment 2 - Lexical Scoping


## Write a short comment describing this function
        ## Input a matrix m1 to store: stored <-makeCacheMatrix(m1); 
        ## Execute inner functions using list notation: 
                ## stored$get() -- show stored matrix m1 
                ## stored$getinverse() -- resolves to NULL
                ## stored$setinverse(m2) -- cache m2 as m1's inverse
                ## stored$getinverse() -- resolves to m2

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


## Write a short comment describing this function 
        ## Calculate inverse of matrix stored$get()

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
        ## Return a matrix that is the inverse of 'x'
}
