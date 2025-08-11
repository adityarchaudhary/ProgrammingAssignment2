## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Programming Assignment 2: Lexical Scoping
## This file contains two functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix:
## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Set the matrix and reset inverse cache
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the stored matrix
    get <- function() x
    
    # Set the inverse in the cache
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the cached inverse
    getInverse <- function() inv
    
    # Return a list of functions for external access
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}

## cacheSolve:
## Computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    # If inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, compute inverse
    mat <- x$get()
    
    # Check if the matrix is invertible
    if (nrow(mat) != ncol(mat)) {
        stop("Matrix must be square to compute inverse.")
    }
    
    inv <- solve(mat, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    inv
}

## Example usage:
# m <- matrix(c(2, 1, 1, 2), 2, 2)
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)  # Computes and caches inverse
# cacheSolve(cm)  # Retrieves cached inverse
