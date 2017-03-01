
## the function sets and gets the value of a matrix
## and sets and gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m << NULL
  }

  ## function get brings the matrix
  get <- function() x
  
  ## function setinverse sets the inverse matrix 
  setinverse <- function(inverse) m <<- inverse
  
  ## function getinverse brings the inverse matrix
  getinverse <- function() m
  
  ## function makeCacheMatrix returns list 
  list (set = set, get = get. setinverse = setinvers, getinverse = getinverse)
}


## this function checks whether the inverse matrix has been calculated
## if yes returns the inverse
## if not it calculates the inverse and then returns it 

## function cacheSolve returns the inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  ## check if the inverse matrix is cached and return message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 

  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

