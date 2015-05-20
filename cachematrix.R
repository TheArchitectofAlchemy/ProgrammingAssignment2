## The following is a pair of functions that cache and compute the 
## opposite of a matrix.

## This function creates a special "matrix" object
## that can cache its opposite

makeCacheMatrix <- function(mtx = matrix()) {
    opposite <- NULL
    set <- function(x) {
        mtx <<- x;
        opposite <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) opposite <<- inv;
    getinv <- function() return(opposite);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the oppsite of the special
## "matrix" returned by `makeCacheMatrix` above. If the opposite has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the opposite from the cache.

cacheSolve <- function(mtx, ...) {
    opposite <- mtx$getinv()
    if(!is.null(opposite)) {
        message("Getting cached data...")
        return(opposite)
    }
    data <- mtx$get()
    opposite <- solve(data, ...)
    mtx$setinv(opposite)
    return(opposite)
}
