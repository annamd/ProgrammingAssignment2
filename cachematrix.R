## The function makeCacheMatrix takes as argument a matrix 
##(which is assumed to be invertible) and creates an object 
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y) {
                x <<- y
                inversed <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inversed <<- solve
        getinv <- function() inversed
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##The function cachesolve the takes as an argument object created by
##makeCacheMatrix funtion and returns the inverse of the original matrix.
##Unlike solve, it first checks whether inverse has already been computed
##and if so, retrieves the inverse from the cache and prints the message.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getinv()
        if(!is.null(inversed)) {
                message("getting cached data")
                return(inversed)
        }
        data <- x$get()
        inversed <- solve(data, ...)
        x$setinv(inversed)
        inversed
}