## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a cache of the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  
  ##Setting the inverse value to NULL 
  matrix_inverse <- NULL
  
  ##Creating function that would allow for the changing of the matrix
  set <- function(y) {
    x <<- y
    ##Returning the inverse value to NULL
    matrix_inverse <<- NULL
  }
  
  ##Assigning "get" to return the matrix stored in x
  get <- function() x
  
  ##Assigning "setInv" to store the value from 
  ## input in the variable "matrix_inverse"
  setInv <- function(inverse) matrix_inverse <<- inverse
  
  ##Assigning "getInv" to store the value from 
  ## input in the variable "matrix_inverse"
  getInv <- function() matrix_inverse
  
  ##Creating a function list so that we have 
  ## access to the various function outputs
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


##This function computes tha inverse of a matrix and stores it in 
## the cache that was previously created. 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  matrix_inverse <- x$getInv()
  
  ## Checking to see if the cache is NULL
  ## If NULL - calculate inverse and cache
  ## Else - retrieve inverse from matrix_inverse
  
  if (is.null(matrix_inverse)) {
    data <- x$get()
    matrix_inverse <- solve(data)
    x$setInv(matrix_inverse)
  }
  else {
    message("getting cached data")
  }  
  matrix_inverse
  
}
