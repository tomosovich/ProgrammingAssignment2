## Put comments here that give an overall description of what your
## functions do

# forms a matrix object that can cache the inverse of the object.
# This returns a list containing functions to set and get the matrix as well as 
# set and get the inverse of the matrix. This becomes the input for the next function (cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
  # starting point, nothing cached
  inv = NULL
  
  set = function(y) {
    # using the new assignment operator to assign the value in a diff environment
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## returns the inverse of the makeCacheMatrix() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  
  # if already calculated
  if (!is.null(inv)) {
    message("cached data")
    return(inv)
  }
  # calculate the inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value in the cache by calling setinv() 
  
  x$setinv(inv)
  
  # returns the final inverse in the cache
  return(inv)
  
}


