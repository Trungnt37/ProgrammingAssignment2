## This function create a matrix, object that can cache its inverse
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the Inverse
## 4. Get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL
	set <- function(y) {
		x <<- y
		cache <<- NULL
	}

	# returns the stored matrix
	get <- function(){
		x
	}

	# cache the given argument
	setInverse <- function(solve) {
		cache <<- solve
	}

	# get the cached value
	getInverse <- function() {
		cache
	}

	list(set = set, get = get,
		setInverse = setInverse, getInverse = getInverse)
}

## Following function return a matrix that is the inverse of 'x'
## (create by makeCacheMatrix function)

cacheSolve <- function(x, ...) {
	# get the cached value
	inverse <- x$getInverse()

	# if a cached value exists, return it
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	# otherwise get (neu k nhan duoc matran), calculate the inverse and store it
	# in the cache
	matrix <- x$get()
	inverse <- solve(matrix, ...)
	x$setInverse(inverse)

	# return the inverse
	inverse
}