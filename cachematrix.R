makeCacheMatrix <- function(a = matrix()) {
  
  # cached inverse of matrix intialized null value
  inver <- NULL
  
  ## getmatrix for getting /setmatrix for setting matrix
  getmatrix <- function() a
  setmatrix <- function(b) {
    a <<- b
    inver <<- NULL
  }
  
  ## getinver for getter/setinver for setter matrix inverse
  getinver <- function() inver
  setinver <- function(inverse) inver <<- inverse
  
  ## return list of functions for matrix
  list(getmatrix=getmatrix, setmatrix=setmatrix, getinver=getinver, setinver=setinver)
}


# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
# Returns:
#   The inverse of the matrix
cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  inver <- a$getinver()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inver)) {
    message("matrix is already inversed")
    return(inver)
  }
  
  # compute inverse of matrix 
  c <- a$get()
  inver <- solve(c, ...)
  
  # cache inverse
  a$setinv(inver)
  
  # return inverse of matrix
  return(inver)
}
