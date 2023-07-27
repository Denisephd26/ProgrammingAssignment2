## Put comments here that give an overall description of what your
## functions do
## These functions will store a matrix in the cache, then allow you
## to quickly find the inverse of that matrix.

## The makeCacheMatrix stores the selected matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv_mat <<- solve
        getinv <- function() inv_mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix
## stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$getinv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data, ...)
        x$setinv(inv_mat)
        inv_mat
}
