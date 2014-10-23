## Solution to Programming Assignment 2 of R Programming course.
## Heavily inspired by example given in README.md (caching the mean of a vector).

## makeCacheMatrix creates a matrix object with the added 
## functionality of caching the inverse of the matrix.
## makeCacheMatrix takes as input a matrix x (assumed to be 
## invertible) and stores the inverse in xInv.
## makeCacheMatrix has member functions to set and get the 
## input matrix and its inverse.
##
## cacheSolve takes as input a matrix object of type 
## makeCacheMatrix and returns its inverse.
## cacheSolve first checks if the inverse has been cached
## and only proceeds to calculate the inverse (using the 
## solve function) if it has not yet been cached.
## cacheSolve caches the inverse for future calculations.

## Usage
## > source('cachematrix.R')
## > x<-makeCacheMatrix(matrix(c(1,0,0,2),2,2))
## > x$setInverse(matrix(c(1,0,0,0.5),2,2)) ## Optional (test behavior with/without caching)
## > cacheSolve(x)

## makeCacheMatrix creates a matrix object that can cache its inverse.
## It takes as input an invertible matrix x.

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) xInv <<- inv
  getInverse <- function() xInv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve caclulates the inverse of an input 
## makeCacheMatrix object x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInverse()
    if(!is.null(xInv)) {
      message("Retrieving cached inverse")
      return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInverse(xInv)
    xInv
}
