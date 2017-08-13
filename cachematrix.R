## CACHING THE INVERSE OF A MATRIX
## Implementation of two functions in order to get the inverse of a matrix from the caché if
## it has been previously computed, saving time and operations.


## This function implements a matrix object which can caché its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL
  
  set <- function(y){
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) minv <<- inverse
  getinv <- function() minv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the matrix object generated with the function placed above. 
## It gets the inverse from the "caché" if possible, otherwise it's computed with solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  
  if (!is.null(minv)){
    message("getting cached inverse")
    return(minv)
  }
  
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv
}
