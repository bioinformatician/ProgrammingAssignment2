## Christian Peikert

## The function creates a "matrix" object with getter and setter methodes. This function object can store 
## the basic matrix as well as a inverted version of the basic matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initialize of the variable for the invertible matrix
  s <- NULL
  
  ## method to the matrix which should be inverted. The invertible matrix variable were set to NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## return the matrix which should be inverted
  get <- function() {
    x
  }
  
  ## method to set the invertible matrix
  setsolve <- function(solve) {
    s <<- solve
  }
  
  ## return the invertible matrix
  getsolve <- function() {
    s
  } 
  
  ## List of function
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function returns a inverted matrix. If the basic matrix were allread inverted the inverted matrix will return
## else the inverted matrix will be compute

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x[['getsolve']]()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x[['get']]()
  s <- solve(data, ...)   ## compute the inverted matrix of data
  x$setsolve(s)           ## stores the inverted matrix in the matrix object
  x$getsolve()            ## returned of the inverted matrix by the matrix object (makeCacheMatrix)
}



m <-rbind(c(2,1,1), c(1, 2, 0), c(1,0,2))  ## example matirx
matrix_object <-makeCacheMatrix(m) ## initialize matrix object
matrix_object[['get']]()       ## return of the matrix m
matrix_object[['getsolve']]()  ## return NULL because the inverted matrix wasn't computed jet
cacheSolve(matrix_object)      ## computation of the inverted matrix
matrix_object[['getsolve']]()  ## return of the inverted matrix 
