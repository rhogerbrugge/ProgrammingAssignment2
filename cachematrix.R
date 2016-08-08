## Coursera R programming
## Peer Graded Assignment: Programming Assignment 2: Lexical Scoping

## Below are the functions with comment

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	    	cachedmatrix <- NULL
        set <- function(y) {
                x <<- y
                cachedmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inversed) cachedmatrix <<- inversed
        getinverse <- function() cachedmatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		    cachedmatrix <- x$getinverse()
        if(!is.null(cachedmatrix)) {
                message("getting cached inversed matrix")
                return(cachedmatrix)
        }
        data <- x$get()
        inversedmatrix <- solve(data, ...)
        x$setinverse(inversedmatrix)
        inversedmatrix ## Return a matrix that is the inverse of 'x'
}
