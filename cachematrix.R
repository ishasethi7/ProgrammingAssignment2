## Purpose: Cache the inverse of an invertible matrix

## This function creates a special matrix object that caches its inverse. 
## It does not make any calculations. 
## It just stores a list of functions (first defines them, then stores them in a list)

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
## Function that changes the matrix stored in the main function (instead of just the set function) by using: "<<-" 
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 ## Function that returns the matrix x stored in the main function
 get <- function() x 
## Function stores the input value in variable inv in the main function
  setinv <- function(solve) inv <<- solve
## Function returns the variable inv stored in the main function
 getinv <- function() inv
## Stores all the defined functions in a list
 list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special matrix returned by above function. If matrix hasnt changed
## and inverse exists, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Gets the inverse matrix (using getinv()) stored in makeCacheMatrix and if it is already computed, just prints the matrix
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## else gets the matrix stored in makeCacheMatrix (get()), computes inverse (solve()), returns the value to makeCacheMatrix( using setinv()) and outputs to console
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

