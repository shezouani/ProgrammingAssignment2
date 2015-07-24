## R Programming Assignment 2

## Caching values
## The 2 following functions compute and cache the inverse of a matrix


#' makeCacheMatrix function
#' creates a special "matrix" object that can cache its inverse
#'
#' @param x    a square matrix
#'
#' @export
#' 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) inv <<- inv
        
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#' cacheSolve function
#' This function computes the inverse of the special "matrix" returned by 
#' makeCacheMatrix above. If the inverse has already been calculated 
#' (and the matrix has not changed), then the cachesolve should retrieve 
#' the inverse from the cache
#' 
#' @param x     a square matrix
#' @param ...   other parameters for the Solve function
#' 
#' @return Return a matrix that is the inverse of 'x'
#' 
#' @export 
#' 
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        mx <- x$get()
        
        inv <- solve(mx, ...)
        
        x$setinv(inv)
        
        inv
        
}
