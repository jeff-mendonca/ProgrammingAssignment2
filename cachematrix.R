## Creating 2 functions:
##	makeCacheMatrix -- a constructor function that makes a cache matrix that
##				 can cache the inverse of the matrix after it is first
##				 computed
##
##	cacheSolve -- function takes the cache matrix returned from the 
##			  constructor function and returns the matix inverse using 
##			  the Solve function.  If solve() has already been executed, 
##			  it will return the cached value.
##
## 

## Return a cache matrix that can retrieve and cache the inverse of the 
## matrix using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)

}


## cacheSolve returns the inverse of the cache matrix x.  It will execute 
## solve() once on the first invocation and then return the cached value 
## thereafter.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s	
}
