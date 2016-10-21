## File:- cachematrix.R
## This file contains a pair of functions - 'makeCacheMatrix' &
## 'cacheSolve' - implemented for 'Caching the Inverse of a Matrix'.
## The function 'makeCacheMatrix' creates a special "matrix" object
## that can cache its inverse. The function 'cacheSolve' computes the
## inverse of the special "matrix" returned by 'makeCacheMatrix'.


## The function 'makeCacheMatrix' creates the special matrix object that can
## cache its inverse. It provides the following functionalities:
## 1. setMatrix:- Used to set new values to the already exisitng object
## 2. getMatrix:- Used to get the existing values in the object
## 3. setInverse:- To set the Inverse of the matrix in the object
## 4. getInverse:- To get the Inverse of the matrix from the object

makeCacheMatrix <- function(x = matrix()) {
    inverseVal <- NULL      # Initialize the inverse matrix as NULL
    setMatrix <- function(y) # Function to set any value specified
    {
        x <<- y
        inverseVal <<- NULL
    }
    getMatrix <- function()    # To return the specified matrix
    {
        x
    }
    setInverse <- function( inv ) # To set the inverse value of the matrix
    {
        inverseVal <<- inv 
    }
    getInverse <- function()    # To return the cached inverse matrix
    {
        inverseVal
    }
    list( setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse, getInverse = getInverse )
}


## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the 'cachesolve' should retrieve
## the inverse from the cache.
## Return a matrix that is the inverse of 'x'
## NB: This function assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
    invMatrix <- x$getInverse()
    # Check whether the inverse matrix of the input matrix is null or not
    if( !is.null( invMatrix ))
    {
        # Inverse is not null. So, get the cached value
        message( "Getting the cached value for inverse matrix" )
        return( invMatrix )
    }
    # No cached value for the input matrix
    # Calculate the inverse matrix
    invMatrix <- solve( x$getMatrix())
    # Set the calculated inverse matrix to the input data
    x$setInverse( invMatrix )
    # Return the inverse of the matrix
    invMatrix
}
