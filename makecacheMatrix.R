makeCacheMatrix <- function(x = matrix()) {
  ## Initializating the inverse matrix with a NULL value.
  cache <- NULL 
  
  ## Create the matrix in the working environment. Passing information from an environment
  ## to another one.
  set = function(y) {
    x <<- y
    cache <- NULL
  }
  
  ## Get the value of the matrix the user wants to be inverted. This information will be 
  ## used by the cacheSolve function at the line 67.
  get <- function() x
  
  ## Invert the matrix and store in cache. When the cacheSolve calculates the inverse 
  ## matrix, this function will be used to cache the calculation. Then, if the user needs
  ## to invert the same matrix, it will be cached - no need to be calculated.
  
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  ## Get the inverted matrix from cache and set it to the function. If the matrix has 
  ## not been inverted, cache variable would have been NULL. Else, this information
  ## will be stored in cache variable.
  getInverse <- function() cache
  
  # Return the created functions to the working environment
  list (set = set, 
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}