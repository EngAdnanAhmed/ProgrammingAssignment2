## Put comments here that give an overall description of what your
## functions do

## "A pair of functions that cache the inverse of a matrix"

## Write a short comment describing this function

## "Creates a special matrix object that can cache its inverse"
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  i <- NULL
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    i <<- NULL
  }
  
  ## Method thet get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- m$getInverse()
  
  ## Just return the inverse if its already set
  
  if( !is.null(x) ) {
    message("getting cached data")
    return(x)
  }
  
  ## Get the matrix from our object
  data <- m$get()
  
  ## Calculate the inverse using matrix multiplication
  x <- solve(data) %*% data
  
  ## Set the inverse to the object
  m$setInverse(x)
  
  ## Return the matrix
  x
}