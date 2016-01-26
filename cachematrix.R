## These two functions are designed to take in a square invertible 
## matrix and create a cached version of that matrix. The first function 
## creates the cached matrix and the second function will either pull 
## that cached version or create a new one.

## The makeCacheMatrix function should:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    value <- NULL
    set <- function(y) {
        x <<- y
        value <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) value <<- inverse
    getinverse <- function() value
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function should:
## (1) set the "value" to the getinverse() function from makeCacheMatrix()
## (2) check to see if the "value" is NULL or contains a value
## (3) If it contains a cached value, return the value with a message
## (4) If it doesn't, solve for the inverse of the matrix and return the value

cacheSolve <- function(x, ...) {
    value <- x$getinverse()
    if(!is.null(value)) {
        message("Retrieving cached inverse!")
        return(value)
    }
    data <- x$get()
    value <- solve(data)
    x$setinverse(value)
    value
}