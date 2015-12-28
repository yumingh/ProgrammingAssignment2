## The two functions below, makeCacheMatrix() and cacheSolve() will create the matrix object and catche the inverse of the matrix.
## It will save lots of time and avoid to compute the result repeatedly.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## set the matrix to null first
    mat <- NULL
    ## set the matrix by using <<- operator, so that it can be used in other environment
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    ## set the inverse of the matrix, also using <<- operator, so that it can be used in cacheSolve() later
    setinverse <- function(inverse) mat <<- inverse
    getinverse <- function() mat
    ## return a list including the following functions: set the matrix, get the matrix, set the inverse, get the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getinverse()
    ## If the inverse has already been calculated (and the matrix has not changed)
    if(!is.null(mat)) {
    ## then it retrieves the inverse from the cache
        message("getting cached data")
        return(mat)
    }
    ## otherwise, it calculates the inverse and returns the inverse result in the cache
    matrixdata <- x$get()
    mat <- solve(matrixdata, ...)
    x$setinverse(mat)
    mat
}
