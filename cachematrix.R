## This pair of functions calculate the inverse of a matrix but in a way that results
## are cached.

## makeCacheMatrix function creates a special type of matrix object, a list that
## contains setter and getter functions for matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL 
	set <- function(y){
		x <<- y
		i <<- NULL
		}
	get <- function() x
	setInverse <- function(solve) i <<- solve
	getInverse <- function() i
	return(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


## cacheSolve function computes the inverse of a matrix (makeCacheMatrix object), by first 
## checking the cache for a computed result before trying to calculate. In case of missing cache
## result it solves for inverse and sets the cache for future computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	#Try to get cached data if any...
	i <- x$getInverse()
	if (!is.null(i)){
		message('returning previously cached data')
		return(i)
		}
	#In case of no cache...
	#get matrix
	data <- x$get()
	#compute it's inverse
	i <- solve(data,...)
	#set the result in cache
	x$setInverse(i)
	#return the result
	return(i)
}

##Possible improvement... dumping and reading cache from files instead of memory?
