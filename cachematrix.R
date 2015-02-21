## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix to null
  inv <- NULL
  
  ##  function to set the matrix and assign the inverse to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## function to get the matrix
  get <- function() {
    x
  }
  
  ## function to assign the inverse matrix
  setinverse <- function(inverse){
    inv <<- inverse
  } 
  
  ## function to get the inverse matrix
  getinverse <- function(){ 
    inv
  } 
  
  ## return the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve calculates the inverse of the special "matrix" created 
## with the above function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Look up cache to get the cached value of inverse 
  inv <- x$getinverse()
  
  ##check to see if the inverse has already been calculated. 
  ## If so, return the inverse from the cache.
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  ## set data to the matrix
  data <- x$get()
  
  ## call function solve set inv to inverse of the matrix
  inv <- solve(data, ...)
  
  ## save the result in cache
  x$setinverse(inv)
  
  ## return the inverse
  inv
}
