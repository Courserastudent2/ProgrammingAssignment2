
## some code to fill a 5x5 matrix with random numbers
## used for own testing purposes, not necessary for code
	
##	randomnumbers <- sample(0:9, 25, replace=TRUE)
##	x <-matrix(nrow=5, ncol=5, data=randomnumbers)
##	x

## function makeCacheMatrix
## returns a list of different functions, used for storing cached values
makeCacheMatrix <- function(x = matrix()) {
	solution <- NULL
	set <- function(y){
	        x <<- y
	        solution <<- NULL
	}

	get <- function() x
	setInverse <- function(solve) solution <<- solve
	getInverse <- function() solution 
	
	list(set=set, get=get,
	setInverse = setInverse
	getInverse = getInverse)

	
}


##cacheSolve uses the above functions for making the inverse of a matrix
##It checks if the inverse of the matrix is already computed
##if the inverse is already known, it returns the inverse
##if the inverse is not yet known, it calculates the inverse and returns it
cacheSolve <- function(x, ...) {

	solution  <-x$getInverse()
	if(!is.null(solution)) {
	        message("getting the cached data")
	        return(solution)
	}
	        data <- x$get()
	        solution <- solve(data, ...)
	        x$setInverse(solution)
	        return(solution)
}
