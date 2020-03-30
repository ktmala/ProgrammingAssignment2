## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeCacheMatrix, creates an invertible matrix object and 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x 
        setInverse <- function(solve) inv <<- solve 
        getInverse <- function() inv 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

## This second function, cacheSolve, computes the inverse of the invertible matrix
## object returned from makeCacheMatrix. If the inverse of the invertible matrix
## has already been calculated and the matrix is not altered, then the cacheSolve
## function below retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
