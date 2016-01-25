#In case of repeated computation, R can cache the computed result and use it later if needed.

#first function is makeCacheMatrix. MakeCacheMatrix creates a matrix and also can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

#Second function is CacheSolve. CacheSolve computes the inverse of the matrix returned by makeCacheMatrix function.
#If the inverse is cached, it retrives inverse of that matrix from cache. Otherwise, it computes the inverse of that matrix.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
