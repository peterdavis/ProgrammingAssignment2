## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix allows for a list to be created that represents a set of functions  
# to allow a matrix and its inverse to be get or set

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function
# cacheSolve will solve the inverse of the matrix set in makeCacheMatrix
# if the inversed matrix exists in cache the it will retrieve the cache
# If not it will calculate the inverse and set it in the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
    
}
