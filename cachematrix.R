## makeCacheMatrix and cacheSolve are a pair of functions wrapped around
## the standard matrix data type in R. They allow to store the result of
## a matrix inversion:
## The first time a call to cacheSolve is made
## the result is stored in a cache, so that subsequent calls only require
## to pull the stored value (as opposed to recomputing the inverse of a matrix,
## which can be costy).
## ------------------------

## Usage/Examples:
## A) my_matrix<-makeCacheMatrix( matrix(rnorm(1000*1000),ncol=1000) )
## B) The 2nd line should be much faster than the 1st:
## cacheSolve(my_matrix)
## cacheSolve(my_matrix)
## C) The matrix multiplication of cacheSolve(my_matrix) and my_matrix$get() 
## should be (almost) the identity. Hence,
## norm(cacheSolve(my_matrix) %*% my_matrix$get() - diag(1000), "M")
## will return a very small number.

## ------------------------


## MAKECACHEMATRIX:

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions:
## 1 set -> set the value of the matrix
## 2 get -> get the value of the matrix
## 3 setInverse -> set the value of the inverse
## 4 getInverse -> get the value of the inverse 
## (The functions setInverse and getInverse are intended to be used only by cacheSolve)



makeCacheMatrix <- function(x = matrix()) {
    ## return a special "matrix" object
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(newInverse) inverseMatrix <<- newInverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CACHESOLVE:

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
