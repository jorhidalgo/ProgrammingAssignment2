## Jorge Hidalgo - R Programming Assignment 
## The function makeCacheMatrix creates a list with 4 functions one of the functions calculates the inverse of a matrix
## another function retrieves the value saved in the global environment of the inverse matrix
## The function cacheSolve has as a parameter the list created by makeCacheMatrix and returns the inverse matrix
## cacheSolve will calculate the inverse matrix only if it was not calculate previously

## The makeCacheMatrix function receives as parameter a matrix and returns a list with 4 functions



makeCacheMatrix <- function(x = matrix ()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL                                                ## sets variable in global environment
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve                     ## creates function to calculate inverse matrix  
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
