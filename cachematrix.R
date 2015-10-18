
## makeCacheMatrix: function, that really is an object which store a matrix, and its inverse 
## SolveCahe: calculate the inverse of a matrix stored in a makeCacheMatrix object and meke sure
## the value is store as part of the objet to avoid the repeated calculation of the inversion once 
## that has been done once

## makeCacheMatrix receive an invertible matrix and store its values, along with 
## its inverse (orginally NULL), and a set of functions that allow to set and get the 
## real value of the matrix and its inverse value, if it had been set up by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function(){x}
  
  setinverse <- function(matrix_){ inverse <<- matrix_  }
  getinverse <- function(){ inverse }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve will recive an object of type makeCacheMatrix, 
## it will check base on the value of "getinverse" if the inversion 
## of the matri has been already set up, if this is the case, it will return
## the cached value, if not it will calculate the inverse, set it in the input object
## through the setinverse function and return

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'  m <- x$getinverse()
  m <- x$getinverse()
  ## if it has been already set up, return the value
  if(!is.null(m)) {
    message("getting inversed cached data")
    return(m)
  }
  ## if the value is null, get the real value of the matrix, inverse it, and set the value 
  ## with the setinverse function of the makeCacheMatrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
