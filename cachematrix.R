## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(mt = matrix()) {
inverse <- NULL
    set <- function(x) {
        mt <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mt);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mt, ...) {
       inverse <- mt$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- mt$get()
    inverse <- solve(data, ...)
    mt$setinv(inverse)
    return(inverse)
}
