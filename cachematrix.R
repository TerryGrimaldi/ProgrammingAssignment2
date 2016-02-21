## Synopsis: cachematrix.R
##
## "cachematrix.R suplies two functions for caching a Matrix,
## and further calculating its inverse. Calculating the inverse
## of a supplied Matrix is specifically called through the second
## function "cacheSolve". If the Matrix data has not changed
## then the funtion "cacheSolve" supplies the answer from the 
## cache, otherwise the a new inverse is calculated and stored 
## in the cache (replacing the previous cache value).
## 
## The function "makeCacheMatrix" is responsible for creating
## and reading the Matrix through "set" and "get" functions
## respectivitly. Calculating the inverse MAtrix and returning
## its values is achieved through "setSolve" and "getSolve"
## functions respectivitly. All four functions defined within
## "makeCacheMatrix" have scope within this finction.


## Synopsis: makeCacheMatrix
##
## This function is used to storing a Matrix to cache and reading
## it back to the console. It further calls the Solve function 
## for calculating the MAtrix inverse, and rading its value back 
## from cache.
## set      Sets the value of the Matrix to cache (Useage x$set(), 
##          where x is a Matrix)
## get      Gets the MAtrix values from cache (Usage: x$get(), 
##          where x is a Matrix)
## setSolve Calls cacheSolve to calculate the Matrix inverse 
##          (Usage: call via cacheSolve: eg. cacheSolve(x), where x is a Matrix.)
## getSolve Gets the inverse Matrix or returns Null if no 
##          inverse has previous been calculated.
##
## Usage: makeCacheMatrix(x,...)
##
## Arguments:
## x - A square Matrix to cache, and will be used to find  its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## sets initial Matrix to Null
  m <- NULL
  ## Stores the supplied Matrix to cacahe
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## returns the value of the cached Matrix
  get <- function() x
  ## Calls cacheSolve to calculate the Matrix inverse
  setsolve <- function(solve) m <<- solve
  ## Gets the values of the cached inverse if present, other returns Null.
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Synopsis:
## This function calculates the inverse of a cached square Matrix.
## Note the function does not handle a Matrix whoes inverse is singular.
##
## Usage: cacheSolve(x,...)
## 
## Arguments:
## x - A square Matrix stored in cache.
## 
## Output: Returns the inverse of the supplied Matrix and stores in cache.

cacheSolve <- function(x, ...) {
  ## Checks the Matrix data. If unchanged returns the value from cache,
  ## otherwise calculate the inverse.
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    ## if unchanged return cache value.
    return(m)
  }
  ## Finds the inverse of the cached Matrix.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
