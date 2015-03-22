## Functions that take advantage of R's scoping rules to cache the inverse of a 
## matrix and avoid extra time-consuming computations

## This function creates a special "matrix" object that can cache its inverse.
## It provides ways to set and get the values of the matrix and set and get
## its inverse

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL

	set <<- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## `cacheSolve` computes the inverse of the special "matrix" returned by
## `makeCacheMatrix`. If the inverse has already been calculated (and the
##  matrix has not changed), then the inverse from the cache is retrieved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Check if we already have the inverse cached and if so, return it
	m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## If the inverse isn't cached, calculate it, cache it, and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}
