## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. These functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     # sets the initial value to NULL. 
     # Will be set (cached) once calculated and returned in subsequent calls
     inv <- NULL
     
     # sets the value of the matrix
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     # gets the value of a matrix
     get <- function() x
     
     # sets the value of the inverse of the matrix
     setinverse <- function(i) inv <<- i
     
     # gets the value of the inverse of the matrix
     getinverse <- function() inv
     
     # return the functions as a list
     list(
          set=set, 
          get=get, 
          setinverse=setinverse, 
          getinverse=getinverse
          )
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
     # attempt to retrive the inverse from cache
     inv <- x$getinverse()
     
     # see if the inverse was cached. If it was, retrieve and return it.
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }

     # the inverse was not cached, so let's calculate it!     
     # get the matrix data
     data <- x$get()
     
     # calculate the inverse
     inv <- solve(data)
     
     # store it in cache for any future retrievals
     x$setinverse(inv)
     
     # return the inverse value
     inv
}
