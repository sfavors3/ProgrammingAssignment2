##  This function accepts a matrix object as a parameter, inverts the object,
##  and stores it in a cached environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 ## m is set to NULL each time makeCacheMatrix is called                                              
  set <- function(y) {                      ## set(internal method): stores the object to the cached environment
    x <<- y
    m <<- NULL
  }
  get <- function() x                       ## get, setsolve, and getsolve (internal methods) allow external access to object
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,               ## list all internal methods
       setsolve = setsolve,                
       getsolve = getsolve)                
}                                           

## This function returns the object created from the makeCacheMatrix function
## and inverts the object.  If the object is already cached, the cached object
## will be returned, otherwise, the matrix will be inverted and stored in the
## cached environment before being returned

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}