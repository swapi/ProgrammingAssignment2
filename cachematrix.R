## Pair of function for calculating the inverse of matrix.
## Apart of calculating the inverse; it caches the result, so 
## that when next time inverse is called for the same matrix
## cached data will be used.

## Creates special matrix which can cache the inverse 
## of this matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


## Returns the cached inverse of the matrix if it is already 
## caculated. If it is not cached then inverse is computed, cached and returned.
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse of matrix")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}

## usage:
## > source('cachematrix.R')
## > cm <- makeCacheMatrix(actualMatrix)
## > inv <- cacheSolve(cm)
