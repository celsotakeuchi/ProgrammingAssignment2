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
