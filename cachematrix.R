## 1. Overall description of what the functions do:

## When used together these functions give you the functionality 
## to calculate the inverse of a matrix, cache this inverse, 
## recall the cached inverse, and set a new matrix that can
## then be cached.


## 2. Short comment describing the makeCacheMatrix function:

## The makeCacheMatrix function creates a list of functions
## that can be used to cache the inverse of a matrix object.
## By setting this list into an object you can easily:
## SET the matrix to be cached in the object 
## GET the matrix that has been cached in the object
## SET the INVERSE to be cached in the object (but not solve it)
## GET the INVERSE that has been cached in the object
## By making into a list, it is easier to recall the functions using $

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, 
           get = get, 
           setinverse = setinverse,
           getinverse = getinverse)
}

## 3. Short comment describing the cacheSolve function:

## The cacheSolve function does the calculations to return
## a matrix that is the inverse of 'x'. It does this by first
## checking to see if there is already an inverse cached in the object,
## if there is it returns a message and the cached inverse, if there is not
## it solves to find the inverse and then sets that inverse to cache.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
      return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m      
}
