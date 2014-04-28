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
    }  # set the matrix in cache and set the cache to null
    
    get <- function() x # return the original matrix
    
    setinverse <- function(solve) i <<- solve  #set the inverse into cache
    
    getinverse <- function() i  #return the inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   #set the list function interfaces
    
}


## Write a short comment describing this function
# cacheSolve will solve the inverse of the matrix set in makeCacheMatrix
# if the inversed matrix exists in cache the it will retrieve the cache
# If not it will calculate the inverse and set it in the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse() #check the cache for an existing inverse. 
    
    if(!is.null(i)) {   # If  exists, return the inverse
        message("getting cached data")
        return(i)
    }
    
    data <- x$get() #retrieve the matrix from the list
    
    i <- solve(data, ...) #calculate the inverse
    
    x$setinverse(i)  # call the inverse set function on the list
    
    i   #return the inverse
    
}
