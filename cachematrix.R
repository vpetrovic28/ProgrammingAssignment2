## Cache the inverse of a matrix, if this already done return the value 

## Function makeCacheMatrix creates a  matrix object in cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
        setSolve = setSolve,
        getSolve = getSolve)
}


## computes the inverse of the matrix returned by makeCacheMatrix if the previously is not compute or not changed
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if (!is.null(m)){
        mb <- makeCacheMatrix()
        if (mb == x){
            return (m)
        }
    } 
    m <- makeCacheMatrix(x)
    x$setSolve(m)
    return (m)
}
