## Put comments here that give an overall description of what your
## functions do
## ----------------------------------------------------------------------------
## Rationale of below functions:
## Inverse of a matrix can be expensive to compute everytime.
## And especially so when the matrix remains the same.
## So, the functions below essentially create a new type of object which holds
## matrix data and also functions that operate on that data.
## When function makeCacheMatrix is invoked it will return that new object.
## That new object is a list containing functions that can be called on the
## stored matrix data.
## ----------------------------------------------------------------------------
## Write a short comment describing this function
## ----------------------------------------------------------------------------
## makeCacheMatrix function creates a special matrix object which holds the 
## matrix and the value of its inverse.
## Moreover, once this object is created with a specific matrix object, 
## the inverse can be set and get.
## Also, the matrix data can be set again to different data values.
## This function essentially returns a list object containing functions 
## for doing all of the above operations on this object.
## ----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL ## this is needed since if data changes, reset inverse
	}
	
	get <- function () x
	set_inverse <- function(inv) inverse <<- inv
	get_inverse <- function() inverse
	list(set=set, 
		 get=get, 
		 set_inverse=set_inverse,
		 get_inverse=get_inverse)
}

## ----------------------------------------------------------------------------
## Write a short comment describing this function
## ----------------------------------------------------------------------------
## cacheSolve function returns the inverse of the special matrix object that is 
## passed in.
## It first checks if cached inverse exists.
## If yes it returns that; if not, it computes, caches and returns the newly
## computed inverse value.
## ----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
	inverse <- x$get_inverse()
	if(!is.null(inverse)){
		message("Fetching cached value...")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data) ## calculating the inverse
	x$set_inverse(inverse) ## setting the cached value
	        ## Return a matrix that is the inverse of 'x'
	inverse
}