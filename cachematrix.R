## R Programming - Assignment 2
## These functions c

## This files contains two functions used to cache the inverse of an invertible
## matrix and to retrieve it. Once computed, the inverse matrix remains 
## available and can be used without another computation.

## This function returns an object that can cache an inverse matrix from a
## matrix given in input.
## The argument "x" is the input matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Setting the input matrix and initializing. If a new matrix is set,
    # the inverse matrix gets set to NULL.
    setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Getting the input matrix 
    getMatrix <- function() x
    
    # Setting the inverse matrix
    setInverseMatrix <- function(inverse) inv <<- inverse
    
    # Getting the inverse matrix
    getInverseMatrix <- function() inv
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)	
}


## This function returns the inverse matrix from an object created whith the 
## makeCacheMatrix function. If the inverse matrix has already been computed,
## it is retrieved from the input object. If not, it's calculated and stored
## in this object.

## The argument "x" is a special object returned by the makeCacheMatrix
## function. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Test if the inverse matrix has already been calculated.
    ## If yes, the inverse matrix gets returned.
    inv <- x$getInverseMatrix()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If not, the inverse matrix gets calculated, cached in
    ## the input object and returned.
    inv <- solve(x$getMatrix(), ...)
    x$setInverse(inv)
    inv
}
