## This file contains 2 functions that are used to improve resource intensive inverse matrix 
## calculation process. Instead of computing inverse every time we need it, we'll be storing it
## in a specially created list instead. This way if the value has already been computed, it will be
## immediately returned without going through the process of recomputing it.

## The first function take a normal invertible square matrix and caches its value
## in addition when the inverse is computed, the function will also store it.
## Thus the function allows to set or get the original matrix as well as set or get the inverse of
## the matrix and all 4 values are cached for future use.
## if the original matrix is re-set using "set" command, the cached version of the matrix 
## gets reset to NULL and will need to be recomputed.

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialize the value of private matix variable to NULL
    m <- NULL
    
    ## here the value of the passed in matix is cached and the cached calculated m value is reset 
    ## to NULL we assume that the matrix provided is invertible and do not do any checking for that
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## this function simply returns the cached copy of the passed in invertible matrix
    get <- function() x
    
    ## this function takes the passed in inverse of the matrix and caches it
    setinverse <- function(inverse) m <<- inverse
    
    ## this function retrieves the value of the inverse matrix from cache and returns it
    getinverse <- function() m
    
    ## a list of pointers to the 4 functions is returned as the result of calling the function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function takes the special list created by the makeCacheMatrix function and checks
## whether the cached inverted matrix already exists for the given matrix.
## If it exists, the function returns the value of the inverted matrix
## If the value of the inverse of the matrix has not yet been computed, the function computes it,
## stores it into cache and returns the value of the inverse matrix.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    ## First let's see if the inverse of the matrix has already been calculated and cached
    ## to do this we retrieve the value from cache and check if it's NULL or not
    m <- x$getinverse()
    
    ## if the value of the cached inverse matrix is not NULL, we retrieve it and return the value
    if(!is.null(m)){
        message("gettign cached data")
        return(m)
    }
    
    ## if the value of inverse matrix has not yet been computed and cached
    ## then calculate it and cache it
    ## retrieve the initial iput data for the invertible matrix
    data <- x$get()
    
    ## compute an inverse of a square invertible matrix
    m <- solve(data, ...)
    
    ## once computed, cache the computed value for the inverse matrix
    x$setinverse(m)
    
    ## return the inverse of the matrix
    m
}
