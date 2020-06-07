## These functions create a matrix object which caches a calculated
## matrix inverse value, and retrieve the cached inverse where available
## to save resources. 

## This return a matrix object which may cache the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(new_mat) {
        x <<- new_mat
        inverse <<- NULL
    }
    set_inverse <- function(inv) {
        inverse <<- inv
    }
    get <- function() {
        return(x)
    }
    get_inverse <- function() {
        return(inverse)
    }
    return(list(set = set, set_inverse = set_inverse, 
                get = get, get_inverse = get_inverse))
}

## This function either returns a cached inverse value of a matrix object x,
## or calculates the inverse and stores it in the cache. 

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if (!is.null(inverse)) {
        message("Retrieving cached inverse:")
        return(inverse)
    }
    mat <- x$get()
    mat_inverse <- solve(mat)
    x$set_inverse(mat_inverse)
    return(mat_inverse)
}
