## The two functions work together in order to cache an
## inverse of a matrix and then either calculate it or 
## retrieve the cached version if the matrix has not changed


## To use it run:
## > m <- matrix(c(2,1,5,3),2,2)
## > a <- makeCacheMatrix(m)
##
## >  cacheSolve(a) ## first run calculates inverse
##
## [,1] [,2]      
## [1,]    3   -5
## [2,]   -1    2
##
##
## > cacheSolve(a) ## second run gets inverse from "cache"
## 3-1-52
## getting cached data
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2

## makeChacheMatrix creates a vector that
## contains a list of function to set/get the matrix
## and it's inverse. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) i <<- inverseMatrix
  getInverseMatrix <- function() i
  list(set = set, 
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix
  )
}

## cacheSolve takes as a parameter the makeChacheMatrix
## functions list in order to calculate it's inverse.
## If the inverse has already been calculated and
## the matrix has not changed then cacheSolve returns
## the cached inverse else it calculates the inverse.
##
## We assume the provided matrix is invertible
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverseMatrix()
  message(i)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverseMatrix(i)
  i
}