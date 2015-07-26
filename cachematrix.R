## The following is a pair of functions that cache
## the inverse of a matrix.
## 
## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
##
## cacheSolve computes the inverse of the special
## "matrix" object returned by makeCacheMatrix. 
## If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve
## retrieves the inverse from the cache, otherwise
## calculates it and caches it.

## 
## makeCacheMatrix function: 
## 	makeCacheMatrix takes as its argument a matrix
##	and creates a special "matrix" object that can
##	cache its inverse
##
##	This special "matrix" object returns a list
##      of functions that can be performed on the object:
##		1.  "get" gets the value of the matrix
##		2.  "set" sets the value of the matrix
##		3.  "setInverse" sets the value of the inverse matrix
##		4.  "getInverse" gets the value of the inverse matrix
##
##	makeCacheMatrix returns a list of special "matrix" object functions

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL				## initialize local inverse to empty/Null.
	set <- function(y) {
		x <<- y				## cached matrix x defined outside function (will be in Global Evironment) is set to the value of matrix x passed in as the argument
		i <<- NULL			## cached inverse "i" defined outside the function (will be in Global Environment) is initialized to Null (empty)
	}
	get <- function() x			## returns cached value of matrix
	setInverse <- function(solve) i <<-solve	## value of Inverse Matrix of arg x is cached in m in Global Environment
	getInverse <-function() m			## return content of cached Inverse Matrix (cached in Global Environment)
	list(set = set, get = get,		## return the list of special "matrix" object functions 
	setInverse = setInverse, 
	getInverse = getInverse)
}



### cacheSolve function: 
## 	cacheSolve takes as its argument a special "matrix"
##	object created by the makeCacheMatrix function and 
##	computes the Inverse of the special "matrix" returned
##	by the by the makeCacheMatrix function.  However, if
##	first checks to see if the Inverse has already been 
##	computed.  If so, it gets the Inverse from the cache 
##	and skips the computation.  Otherwise it calculates 
##	the Inverse of the data and sets the value of the 
##	Inverse in the cache via the setInverse function.
##	
##
##	cacheSolve function returns either the cached (if 
##	available) or computed Inverse of the data in the 
##	special "matrix" object.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()			## Get current value of m to value of m accessed by getmean function of special "matrix" object
	if (!is.null(m)) (			## If Inverse is already cached, return the cached value
		message("getting cached data")
	return(m)
	}
	data <- x$get()				## Otherwise, get the value of the matrix
	m <- solve(data, ...)			## calculate the inverse of the matrix
	x$setInverse(m)				## cache the value of the inverse of the matrix
	m					## return the Inverse
}
