## Put comments here that give an overall description of what your
## functions do

## "A pair of functions that cache the inverse of a matrix"

## Write a short comment describing this function

## "Creates a special matrix object that can cache its inverse"
makeCacheMatrix <- function(x = matrix()) {
  
  invM <- NULL
  
  set <- function(y){
    x <<- y
    invM <<- NULL
  }
  
  get <- function() x
  
  setinvM <- function(inverse) invM <<- inverse
  
  getinvM <- function() invM
  
  list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
  
}


## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- m$getinvM()
  
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
  m$setinvM(x)
  
  ## Return the matrix
  x
}