## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special matrix object that can cache its inverse using setinverse and getinverse
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y) {
		x<<-y
		m<<-NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}


## Write a short comment describing this function
## Computes the inverse of the special matrix supplied using the makeCacheMatrix function. If the inverse has already been calculated and the matrix has not changed, the inverse is retrieved from the cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m<-x$getinverse()
        if(!is.null(m)){
        	message("Getting cached data")
        	return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setinverse(m)
        
}
