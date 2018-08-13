# Assignment 3

# Instructions
# Write the following functions:

# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.



## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #store i as a NULL object
    set <- function(y) {
        x <<- y 
        i <<- NULL
    }
    get <- function() x # get x from the parent environment
    setinverse <- function(inverse) i <<- inverse # calculate the matrix inverse and store as i in parent
    getinverse <- function() i # get i from the parent environment
    list(set = set, # name the objects so you can call on them later
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function

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
