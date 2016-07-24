## This code demonstrates scoping rules of the R language.
## If the contents of a matrix are not changing, it may make sense to cache the 
## value of the inverse matrix so that when we need it again, it can be looked 
## up in the cache rather than recomputed.
## <<- operator is used to assign a value to an object in an environment that 
## is different from the current environment




## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## calculates the inverse of the special "matrix" created with the makeCacheMatrix
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached output")
    return(inverseMatrix)
  }
  data <- x$get()
  
  if(det(data) == 0){
    stop("Inverse can not be computed determinant of input matrix is 0")
  }
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  inverseMatrix
}