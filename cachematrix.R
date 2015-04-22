## Cache the inverse of a matrix, if this already done return the value 

## Function makeCacheMatrix creates a  matrix object in cache

makeCacheMatrix <- function(x = matrix()) {
	mm <- NULL
	set <- function(y){
		x <<- y
		mm <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) mm <<- inverse
	getinverse <- function() mm 
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix if the previously is not compute or not changed
        ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if (!is.null(m)){
		mb <- x$get()
		if (mb == x){
			return (m)
		}
	} 
		mb <- x$get()
	mi <- solve(mb)
	x$setinverse(mi)
	return (mi)
}
