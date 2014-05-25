## The two functions below can be used to create an object that stores the   
## value of a matrix and caches the value of the inverse of this matrix.

## The function makeCacheMatrix creates a matrix like object that can 
## store its inverse. makeCacheMatrix function contains a list with 
## functions to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(input){
		x <<- input
		inverse <<- NULL
	}
	get <- function(){
		x
	}
	setinverse <- function(inv){
		inverse <<- inv
	}
	getinverse <- function(){
		inverse
	}
	list(set = set, get = get,
    	setinverse = setinverse,
    	getinverse = getinverse)
}


## The function below solves for the inverse of the matrix-like object 
## created by the function makeCacheMatrix. The cacheSolve function 
## checks if the inverse has already been calculated. If yes, the 
## inverse matrix is taken from cache. Otherwise, the inverse matrix 
## is calculated and sets the value of the inverse in the cache with 
## setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
	if (!is.null(inv)){
		## This piece of code returns the inverse matrix 
		## if it is already in cache
		message("Getting cached inverse matrix")
		return(inv)
	}
	## This piece of code computes the inverse and stores 
	## it in cache if it wasn't done before 
	mat <- x$get()
	inv <- solve(mat)
	x$setinverse(inv)
	inv
}
