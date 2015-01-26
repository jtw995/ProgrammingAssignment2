## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix takes in a matrix and returns a list of functions to get, set,
# and to get and set inverted maxrices.  It holds the values of the inversions 
# to save time in the future. 
# cacheSolve takes in a matrix and returns the inverse and caches it using 
# makeCacheMatrix's setinvmatrix function.


# makeCacheMatrix will make a caching matrix that we can use with cacheSolve to
# save the work of inverting the matrix so that when we later need the inverted 
# matrix we can refer to the past computed value and save time

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(invmatrix) m <<- invmatrix
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## cacheSolve take in a matrix, x and returns an inverted matrix and caches it
## using the function setinvmatrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinvmatrix(m)
    m
}
