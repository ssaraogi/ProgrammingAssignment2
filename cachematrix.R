## Below are two functions that are used to create a special object that stores
## a matrix and caches the inverse of the matrix.

## makeCacheMatrix creates a special "matrix" that can cache its inverse.
## The special "matrix" is really a list containing functions to
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setinverse: set the value of the inverse of the matrix
## 4. getinverse: get the value of the inverse of the matrix

makeCacheMatrix <- function(matrix = matrix()) {
    matrix_inv <- NULL
    
    ## set the value of the matrix and unsets the inverse of the newly set
    ## matrix
    set <- function(matrix_in) {
        matrix <<- matrix_in
        matrix_inv <<- NULL
    }

    ## get the value of the matrix
    get <- function() matrix

    ## set the calue of the inverse of the matrix
    setinverse <- function(matrix_inv_in) matrix_inv <<- matrix_inv_in

    ## get the value of the inverse of the matrix
    getinverse <- function() matrix_inv

    ## return the list containing the above functions
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## The following function calculates the inverse of the special "matrix"
## returned by makeCacheMatrix. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the matrix and
## sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(cache_matrix, ...) {
    ## Return a matrix that is inverse of 'cache_matrix'

    ## Get the inverse of the matrix
    matrix_inv <- cache_matrix$getinverse()

    ## Check if the inverse of the matrix is already calculated and cached
    if (!is.null(matrix_inv)) {
        message("getting cached data")

        ## return the cached inverse of the matrix
        return(matrix_inv)
    }

    ## Calculate the inverse of the matrix and cache it

    ## Get matrix from cache matrix
    matrix <- cache_matrix$get()

    ## Calculate inverse of the matrix
    matrix_inv <- solve(matrix, ...)

    ## Cache the inverse of the matrix in the cache matrix object
    cache_matrix$setinverse(matrix_inv)

    ## return the inverse of the matrix
    matrix_inv
}
