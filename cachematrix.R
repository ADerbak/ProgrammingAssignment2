## Andrew Derbak, September 6th, 2017
##
## Create a matrix and solve for its inverse using these two 
## functions.
## MATRICES MUST BE SQUARE!!!

## This function Creates a cached matrix to store for pickup from
## the 2nd function.
## This function NULLs any previous matrix and sets the inverse.

makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y){
                  x <<- y
                  m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## This function reads the makeCacheMatrix function and 
## gets the data from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvers()
          if(!is.null(m)){
            message("getting cached data")
            return(m)
          }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
