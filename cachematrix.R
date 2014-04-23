## Create a cache matrix object to repeatably solve the inverse of 
## the matrix,but onl calculate te inverse onc

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
	set <- function(y) {
    		x <<- y
    		inverse_matrix <<- NULL
	}
 	get_matrix <- function () x
  	set_matrix <- function(solve) inverse_matrix <<- solve
  	get_matrix <- function () inverse_matrix 
  	list(set = set, get = get,
     		set_matrix = set_matrix,
     		get_matrix = get_matrix)
}

## computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve retrieves 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse_matrix <- x$get_matrix()
  	if(!is.null(inverse_matrix)) {
  		message("getting cached data")
   		return(inverse_matrix)
  	}
 	data <- x$get()
  	inverse_matrix <- solve(data, ...)
  	x$set_matrix(inverse_matrix)
  	inverse_matrix
} 
