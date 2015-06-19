##===========================================================================================================
#
# Written by GDaniesJr (modified version of the makeVector and cachemean function) for Coursera 
#- Programming Assignment 2
#
# R v. 3.1.2
#
# The functions below allows you store a matrix as a "special" matrix and return and cache its inverse
#
##===========================================================================================================

#============================================================================================================
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#============================================================================================================

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(solve) m <<- solve
  getinver <- function() m
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}

#============================================================================================================
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#- If the inverse has already been calculated (and the matrix has not changed), 
#- then the cachesolve should retrieve the inverse from the cache.
#============================================================================================================


cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinver(m)
  m           # Return a matrix that is the inverse of 'x'
}

