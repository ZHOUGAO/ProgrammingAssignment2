## Caching the Mean of a square Matrix (no singular)


## This function give a list of 4 functions, which are 'set','get'
## 'setinverse' and 'getinverse'
## for example, F<-makeCacheMatrix, then F$set('matrix'), to apply function$set

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get <- function() x
	setinverse <- function(matrixinverse) {m<<-matrixinverse}
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The second fuction first searches for the cached inverse of the Matrix
## It will return a matrix that is the invese of 'x' either after calculation
## Or it will print out "getting cached data" and give the inverse cached in environment

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	} 
	data <-x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}