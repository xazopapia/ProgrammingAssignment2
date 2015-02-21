## this is to define the makeCacheMatrix function
## the makeCacheMatrix function is created to calculate the inverse of a matrix, x, by:
## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the value of the inverse
## 4. getting the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

##cacheSolve calculates the inverse of the matrix created with makeCacheMatrix
##before doing that, it checks whether the inverse has already been calculated
##if so, it returns the message "getting cached data" and returns the value of the precalculated inverse.
##if not, it recalls the previous defined function to get the value of the matrix
##and calculates the inverse accordingly

cacheSolve <- function(x, ...){
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
