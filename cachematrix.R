## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  mat <- x
  inv = matrix(c(0),1,1)
  getfunction <- function() mat
  setfunction <- function(y = NULL) {
    mat <<- y 
    inv <<- NULL
  }
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(get = getfunction, set = setfunction, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  
  inv = x$getinv()
  
  # do not compute if the inverse has already been calculated
  if (!identical(inv,matrix(c(0),1,1))){
    
    message("Inverse has already been calculated!")
    return(inv)
  }
  
  # otherwise, compute the inverse 
  mat = x$get()
  inv = solve(mat, ...)
  
  # set the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}