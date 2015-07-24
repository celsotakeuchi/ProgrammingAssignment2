### July, 24rd of 2015

## This function is able to cache the inverted matrix, an operation that is usually time
## consuming. To opmize the process, instead of re-calculate everytime the inverse 
## matrix, those functions will cache the inverse matrix if it has been calculated 
## before. 

## This function creates a special "matrix" object that can cache its inverse. All info
## stored by this function will be used by the cacheSolve function. The first function,
## creates a special "matrix", which is really a list containing a function 
## to:
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse matrix
##    4. get the value of the inverse matrix

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

## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve the 
## inverse from the cache.


cacheSolve <- function(x, ...) {
## Attempt to get the inverse of the matrix stored in cache variable.
      cache <- x$getInverse()
  
## Verify if the return inverted matrix has been already calculated (If Cache Not null).
## Get information from cache and display it to the user.
      if (!is.null(cache)) {
              message("Getting information from previous calculation")
              return(cache)
      }
## Else, calculate the inverse of the matrix since it does not exist. Cache the new
## inverse matrix for the future calculations. (x$setInverse) and return the calculation
## to the final user.
      matrix <- x$get()
      inv <- solve(matrix)
      x$setInverse(inv)
      message("Calculating the inverse matrix")
      return(inv)
}
