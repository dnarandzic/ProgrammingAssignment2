## This file contains functions that cache the inverse of a matrix, which can
## make time consuming operation of calculating the inverse faster in cases
## when this operation should be perfoemed repeadetly.


## makeCacheMatrix function contains four functions:
## 1. set is used to change and cache matrix stored in makeCacheMatrix 
##    function. When called, it also sets inverse matrix in makeCacheMatrix
##    function to NULL.
## 2. get is used to return matrix cached in makeCacheMatrix function
## 3. setinverse is used to set and cache inverse matrix in makeCacheMatrix
##    function. Inverse matrix given in argument of setinverse function can
##    be inverse of a matrix stored in makeCacheMatrix function, but can be
##    some other matrix too.
## 4. getinverse is used to return inverse matrix cached in makeCacheMatrix
##    function.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve function returns inverse matrix cached in ## function of type
## makeCacheMatrix provided in it's argument. If inverse matrix in function
## provided in it's argument is NULL (is not cached), inverse matrix will be
## calculated in this function and cached used setinverse function of
## makeCacheMatrix.

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
