## function makeCacheMatrix cashes the matrix and its inverted matrix; 
## function cacheSolve returnes the cashed inverted matrix,
## or calculates a new one if it was not prewiosly cached


## function makeCacheMatrix caches the matrix and inverted matrix via 
##set and setInvMatrix respectively, and returns the matrix and inverted matrix 
##via get and getInvMatrix and returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invrs) m <<- invrs
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## function cacheSolve returns the casched inverted matrix once being cached.
## if matrix is set as new (new matrix is freshly cached), cacheSolve calculetes
## inverdet matrix, cahces it via calling setInvMatrix, and returns it. 

cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached inverted matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}