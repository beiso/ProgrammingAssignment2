## These functions provide the ability to cache the inverse of a matrix
## so that it does not have to be re-calculated each time. This can 
## improve performance as calculating the inverse can be expensive
## but the inverse of a matrix does not change.
## The functions assume that the inverse of the matrix exists.



## This function creates a special matrix object that is able to cache
## the value of the inverse function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## set the matrix and reset the cached inverse value
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get returns the matrix
    get <- function() x
    
    ## setinverse sets the value of the inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    
    ## getinverse returns the inverse of the matrix
    getinverse <- function() i
    
    ## configure the functions that are available
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a special matrix created using the makeCacheMatrix 
## function and returns the inverse of the matrix using the cached value
## if possible.
## Additional parameters may be passed to the underlying solve function
## using the ... parameters

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## If the inverse has been cached then return it
    if(!is.null(i)) {
        return(i)
    }
    
    ## The inverse has not been cached so calculate and cache it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

