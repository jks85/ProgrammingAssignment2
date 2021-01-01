## This function returns a list containing getters and setters
## for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                   ## initialize inverse as null
  
  set <- function(y){           ## set matrix
      x <<- y                   ## set/super-assign new value for matrix
      inv <<- NULL              ## set/super-assign inverse as null since it is now unknown
    
  }
  
  
  get <- function(){           ## get matrix
      x
  }  
  
    
  set_inverse <- function(inverse){        ## set inverse of matrix
    inv <<- inverse
  }  
    
  get_inverse <- function(){               ## get inverse of matrix
    inv
  }
  
  list(get = get, set = set, set_inverse = set_inverse, get_inverse = get_inverse)
  
}


## This function returns the inverse of a matrix. If the inverse was previously
## computed it is retrieved from the cache, otherwise it uses the previous
## function to set the inverse

cacheSolve <- function(x, ...) {

  inv <- x$get_inverse()                         ## get inverse of matrix
  if(!is.null(inv)){                         ## return inverse if it is not null
    message("getting cached inverse")
    return(inv)
    
  }
  
  mat <- x$get()                             ## if inverse is null get cached matrix
  inv <- solve(mat)                          ## compute inverse
  x$set_inverse(inv)                             ## set inverse in cache
  inv
  
  
  
}
