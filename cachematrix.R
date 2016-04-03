## Functions to set up cache matrix and return the inverse

## Set up the cache matrix as per the given example.
## ie "setting" and "getting" the original matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
                set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}

## Return inverse matrix of input matrix ‘x’

## checks if inverse matrix is empty (does not contain null). 
## if so returns the matrix, otherwise solves the inverse, 
## using a temporary newMat.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                return(inv)
        }
        newMat <- x$get()
        inv <- solve(newMat, ...)
        x$setInv(inv)
        inv
}