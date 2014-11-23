#makeCacheMatrix will return a list of several functions that will:
	#set the value of the matrix, retrieve teh value of the matrix, set the value of the inverse matrix, retrieve the value of the inverse

#cacheSolve will calculate the inverse of a matrix


#makeCacheMatrix will store the value of a cached matrix, retrieve that value and return the matrix after having passed it through the defined functions below
makeCacheMatrix <- function(x=matrix()) {
	#here "inv" will store the value of the cached matrix
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	#retrieve the matrix
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	#returning the matrix with the functions defined above
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

#cacheSolve will determine the the inverse matrix has already been calculated, and if so will return that value.
	#if the value has not been calculated, the function will retrieve the data and pass it through the inverse function and return the values
cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	#if the inverse has already been calculated, this will retrieve that stored data and return it
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	#cacluate the inversed matrix
	data <- x$get()
	inv <- solve(data, ...)
	#set the cache inverse
	x$setinv(inv)
	#retrieve and print to user the values in the cached matrix
	inv
}