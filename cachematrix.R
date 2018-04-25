# Description
# This function creates a special “matrix” object that can cache its inverse.
# If the inverse has already been calculated, then the cachesolve should retrieve
# the inverse from the cache. Computing the inverse of a square matrix can be done
# with the solve function in R. For example, if X is a square invertible matrix, 
# then solve(X) returns its inverse.

## Function: Matrix object that can cache inverse.

makeCacheMatrix <- function(x = matrix())
 {
  	inverse <- NULL
  	set <- function(y)
	{
    		x <<- y
    		inverse <<- NULL
 	}
  	get <- function() x
 	setInverse <- function(solveMatrix) inverse <<- solveMatrix
  	getInverse <- function() inverse
  	list(set = set, get = get, 
			setInverse = setInverse, 
			getInverse = getInverse)
 }


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) 
 {
      inverse <- x$getInverse()
  	if(!is.null(inverse))
	{
    		message("cached data")
    		return(inverse)
  	}
  	data <- x$get()
  	inverse <- solve(data)
  	x$setInverse(inverse)
  	inverse      
}
