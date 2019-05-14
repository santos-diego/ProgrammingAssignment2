## Caching the Inverse of a Matrix - Programming Assignment 2: Lexical Scoping
## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        aux_inv <- NULL
        set <- function(y){
                x <<- y
                aux_inv <<- NULL
        }
        get <- function() x
        set_inverso <- function(solve) aux_inv <<- solve
        get_inverso <- function() aux_inv
        list(set = set, get = get,
             set_inverso = set_inverso,
             get_inverso = get_inverso)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        aux_inv <- x$get_inverso()
        if(!is.null(aux_inv)) {
                message("getting cached data")
                return(aux_inv)
        }
        data <- x$get()
        aux_inv <- solve(data, ...)
        x$set_inverso(aux_inv)
        aux_inv

}
