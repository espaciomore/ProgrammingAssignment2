## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {
    return(x)
  }
  setsolve <- function(s) {
    m <<- s
  }
  getsolve <- function() {
    return(m)
  }
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## <params>
## x: a special matrix returned by makeCacheMatrix
## ...: arguments for function solve
## </params>

cacheSolve <- function(x, ...) {
  ma <- x$getsolve()
  if(is.null(ma)){
    starttime <- Sys.time()
    x$setsolve(solve(x$get(), ...))
    endtime <- timestamp(quiet = TRUE)
    message(paste("Inverse computed in approximately",round(difftime(Sys.time(),starttime,units = "secs"),3), "seconds",sep=" "))
  } else {
    message("Inverse retrieved from cache")
  }
  return(x$getsolve())
}
