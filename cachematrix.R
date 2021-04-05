## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It creates a matrix object that can cache it's inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## It is used to set the value of matrix x
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        ## It is used to get the value of matrix x
        get <- function() x 
        
        ## It is used to set the value of inverse of x
        setinverse <- function(solve) {
            m <<- solve
        }
        
        ## It is used get the value of inverse of x
        getinverse <- function() m
        list(set = set, get = get,
             setinv = setinverse,
             getinv = getinverse)
}


## Write a short comment describing this function
## This function checks whether the inverse matrix value is cached or not and returns the inverse matrix if it exists.
cacheSolve <- function(x, ...) {
        m <- x$getinv() ## Gets the inverse matrix value
        
        ## Condition to check whether the value is present or not
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
          ## If already present returns the value
        }
        
        ## If the value is not calculated, it does so now by using solve() method.
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
