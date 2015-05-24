## Define two functions that allow us to compute the inverse of a matrix and cache
## it such that computing it again is not necessary

## Return a "matrix" that allows you to cash its inverse, 
## input parameter M should be an inversible matrix
makeCacheMatrix <- function(M = matrix()) {
  # initialize W, which will be the inverse of M
  W = NULL
  
  #define a function that returns the current matrix, M
  get <- function(){return(M)}
  
  # defines a function that returns the current value of W
  get_inverse <-function() return(W)
  
  # defines a function to store a matrix as the inverse, W, of M
  set_inverse <- function(inverse) W<<-inverse
  
  # return functions created above for use outside makeCacheMatrix
  interface <- list(get=get, set_inverse=set_inverse, get_inverse=get_inverse)
  return(interface)
}


## Return a matrix that is the inverse of cacheMatrix
## Only computes it the first time, then uses the cached inverse thereafter
cacheSolve <- function(cacheMatrix, ...) {
  # if cacheMatrix's inverse is not NULL, message("getting cached data") and return the cached data
  inv <-cacheMatrix$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
  } else { 
    # else, solve for the matrix's inverse, store it in cacheMatrix, and return it
    inv <-solve(cacheMatrix$get(), ...)
    cacheMatrix$set_inverse(inv)
  }
  return(inv)
}
