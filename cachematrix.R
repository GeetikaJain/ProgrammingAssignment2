
## This program will create a matrix package that will use cache for storing
## inverse of a matrix to save computational time

## Assumptions: Only utilities mentioned in this package will be used for 
## setting and getting the matrix and the inverse of the matrix

## This function creates matrix operations to be used for creating/ modifying/
## computing inverse / fetching inverse from cache / setting inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ## This function will be used for modification of the matrix
  ## Matrix "x" will be set equal to matrix "y" passed to the function
  set <- function(y) {
    x <<- y
    
    ## set the inverse in cache to be NULL
    inverse <<- NULL
  }
  
  ## This function will be used to fetch the matrix x
  get <- function() x
  
  ## Store the computed inverse in cache
  setinverse <- function(solve) inverse <<- solve
  
  ## Get the inverse from cache
  getinverse <- function() inverse
  
  ## Create a list of functions to set/get/setinverse/get inverse of the matrix
  ## this list is returned by the makeCacheMatrix function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  
  ## Fetch the inverse from cache
  inverse <- x$getinverse()
  
  ## If inverse is not NULL it means stored inverse is valid, 
  ## i.e. the matrix has not changed since last inverse computation
  if(!is.null(inverse)) {
    message("getting cached data")
    
    ## return stored inverse
    return(inverse)
  }
  
  ## Else if matrix has changed since last inverse compute, fetch new matrix
  data <- x$get()
  
  ## Compute the new inverse
  inverse <- solve(data, ...)
  
  ## Save the computed inverse to cache
  x$setinverse(inverse)
  
  ## return the computed inverse
  inverse
}