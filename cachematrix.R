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
  
  
  list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv) ## list containing getters and setters

}


## This function returns the inverse of a matrix. If the inverse was previously
## computed it is retrieved from the cache, otherwise it uses the previous
## function to set the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Note: takes list as an input; this will be the list returned by makeCacheMatrix()
  
  inv <- x$get_inv()  # get current value of inverse
  
  if (!is.null(inv)){   # return current value if inverse is not null
      message("getting cached inverse")
      return(inv)
  }
 
 # if inverse is null :
  
  mat <- x$get_mat()   # retrieve cached matrix
  inv <- solve(mat) # solve its inverse
  x$set_inv(inv) # set inverse
  inv
  
}
