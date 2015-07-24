## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix functions
## The first function, makeCacheMatrix creates a special "Matrix", 
## containing a function to
##   set the value of the Matrix
##   get the value of the Matrix
##   set the value of the solve
##   get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
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

## c=rbind(c(1, -1/4), c(-1/4, 1))  
## matrixPr <- solve(c)