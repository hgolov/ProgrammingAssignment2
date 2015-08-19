## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Accept information to create a matrix, instaniate inverse
##to null. Some getters & setters are included

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(data = NA, cols = 1, rows = 1){
        x <<- matrix(data, nrow=rows, ncol=cols)
        inverse <<- NULL
    }
    get <-function() x
    setinverse <- function(inv){ inverse <<- inv}
    getinverse <- function()inverse
    list(set = set, get = get, setinverse= setinverse,
          getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse
    if(!is.null(inv)){
      message("getting cached data")
      return (inv)
    }
    data <- x$get
    inv <-solve(data)
    x$setinverse(inv)
    inv
}
