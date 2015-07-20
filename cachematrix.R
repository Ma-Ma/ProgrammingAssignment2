## These functions create an object that stores a square matrix and caches it's inverse.

## 'makeCacheMatrix' creates a list containing a function to
## 1. set the value of the square matrix x
## 2. get the value of x
## 3. set the value of the inverse of x
## 4. get the value of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setinv <- function(solve) s <<- solve
     getinv <- function() s
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## 'cacheSolve calculates the inverse of the matrix created with 'makeCacheMatrix'.
## It checks to see if the inverse has already been calculated. If it has been, it
## gets the inverse from the cache. Otherwise, it calculates the inverse and sets 
## the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     s <- x$getinv()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinv(s)
     s
}
