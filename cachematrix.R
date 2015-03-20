# The details on each function is described below in the respective areas. Also the instruction on how to execute this program is detailed at the end of this program

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    ## Internal function declaration starts - 1) set, 2) get, 3) setInverseMatrix 4) getInverseMatrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    # setInverseMatrix is responsible for computing inverse of the given matrix
    setInverseMatrix <- function(solve) m <<- solve
    
    getInverseMatrix <- function() m
    
    ## Internal function declaration ends
    
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
    
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'    
    
    
    m <- x$getInverseMatrix()
    
    # Get the resultant inverse matrix if it is already in cache or proceed with computation and return the inverse matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverseMatrix(m)
    m
    
}

# This package can be tested by executing following commands:
# d <- makeCacheMatrix(matrix(1:4,2,2))
# where, the matrix elements or values in the matrix can be changed as per user's convenience
# cacheSolve(d)