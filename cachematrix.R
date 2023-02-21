## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix is used to the special matrix. 
## The function cacheSolve is used to compute the inverse of the special matrix
## that is returned by the function makeCacheMatrix.
## However, if the inverse has already been calculated, then the function
## receives the inverse from the first funciton.

## Write a short comment describing this function
## Returns a special matrix with a containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y # <<- is used to assign a value to an object in an environment 
    # that is different from the current environment.
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, 
       get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Write a short comment describing this function
## Returns the inverse of the special matrix that is returned 
## by function makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if (!is.null(inv)) { # check if inverse has been calculated
    message("getting cached data.")
    return(inv) # skips computation and receives from the cache
  }
  # computation to get inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inv(inv)
  inv
  
}
