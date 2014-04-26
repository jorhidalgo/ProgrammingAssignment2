## Jorge Hidalgo - R Programming Assignment 
## The function makeCacheMatrix creates a list with 4 functions and saves in cache the inverse matrix  
## of the matrix passed as parameter
## The function cacheSolve has as a parameter the list created by makeCacheMatrix and returns the inverse matrix
## cacheSolve will calculate the inverse matrix only if it was not calculate previously

## The makeCacheMatrix function receives as parameter a matrix and returns a list with 4 functions
##  The makeCacheMatrix also calculates the inverse of the matrix passed as parameter and caches the result using 
## the operator <<


makeCacheMatrix <- function(x = matrix ()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve                     ## the inverse matrix is calculated and saved in cache  
  getsolve <- function() m
  list(set = set, get = get,                                  ## create list with 4 functions 
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve functions first validates if it has stored in $getsolve a value different from null
## if the value is different from null then retrieves the inverse matrix that was already stored
## if the value in $getsolve is null then calculates the inverse matrix 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()                                               ## retrieves from the list the value stores in $getsolve
  if(!is.null(m)) {
                     message("getting cached inverse matrix")     ## print message it is using cached inverse matrix
                      return(m)                                   ## returns the value that was already stored
  }
  data <- x$get()                                        
  m <- solve(data, ...)                                           ## inverse matrix not in cache then calculates it
  x$setsolve(m)
  m                                                               ## returns calculate value of inverse matrix 
}
