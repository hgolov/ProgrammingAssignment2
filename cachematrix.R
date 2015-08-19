## Create a matrix object that can cache its inverse
## and a function to return the inverse - from cache if available


##CacheMatrix object definition
##Accept information to create a matrix, instaniate inverse
##as null. Allow users to set matrix, get matrix, set the inverse
## and get the inverse

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


##Accept a CacheMatrix object, if inverse has been calculated, return
##cached result, otherwise calculate, cache & return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if(!is.null(inv)){
      return (inv)
    }
    data <- x$get()
    inv <-solve(data)
    x$setinverse(inv)
    inv
}
