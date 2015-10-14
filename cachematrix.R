## Put comments here that give an overall description of what your
## functions do

## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing 4 functions which:
##     1.  set the value of the matrix
##     2.  get the value of the matrix
##     3.  set the value of the inverse (aka solve)
##     4.  get the value of the inverse (aka solve)

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The following function calculates the inverse of the special "vector"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setsolve`
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## The following bonus function tests to see if everything is correct.
## A matrix multiplied by it's inverse should return identity matrix

cacheTest <- function()
{
    # make a 10x10 matrix with nonzeros in the diagonal
    mat <- matrix(rep(0,100), nrow=10, ncol=10)
    diag(mat) <- rnorm(1:10)

    # make the cache matrix
    cachedMat <- makeCacheMatrix(mat)

    # print the matrix using the cache's get() function
    print("matrix:")
    print(cachedMat$get())

    # print the matrix's inverse using the result of
    # solving it with cacheSolve()
    print("inverse:")
    print(cacheSolve(cachedMat))

    # print the matrix * matrixinverse using the cached version
    print("identity:")
    cachedMat$get() %*% cacheSolve(cachedMat)
}
