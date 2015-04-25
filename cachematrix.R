## Put comments here that give an overall description of what your
## functions do

## This function returns a list containing getters and setters
## for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL  #initialize inverse as null
  
  set_mat <- function(mat){  # set matrix
      x <<- mat
      inv <<- NULL
  }   
  
  get_mat <- function() x  # get matrix
  
  set_inv <- function(inverse) inv <<- inverse   # set inverse
  
  get_inv <- function () inv  # get inverse
  
  
  list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv)

}


## This function returns the inverse of a matrix. If the inverse was previously
## computed it is retrieved from the cache, otherwiseit uses the previous
## function to set the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$get_inv()  # get current value of inverse
  
  if (!is.null(inv)){   # return current value if inverse is not null
      message("getting cached inverse")
      return(inv)
  }
  
  mat <- x$get_mat()   # select matrix
  inv <- solve(mat)
  x$set_inv(inv) # find inverse using solve and set it
  inv
  
}
