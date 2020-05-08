## Functions computing inverse of matrix
## This function creates object that can cache matrix inverse

makeCacheMatrix <- function(x = matrix()) {
 
   inv <- NULL
   set <- function(y) {
       x <<- y
       inv <<- NULL
    }
   get <- function() x
   setinv <- function(invs) inv <<- invs
   getinv <- function() inv
  
   list(set = set, get = get,
         setinv = setinv,
          getinv = getinv)
}


## This function calculates the inverse of the matrix
## returned by makeCacheMatrix. If inverse is already done, 
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
