## Put comments here that give an overall description of what your
## functions do
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Create matrix cache used for inverse calc

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL #set as null inverse 
  
  ## set matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate and retrieve the inverse matrix from the cache

cacheSolve <- function(x, ...) {
  ## Return matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse IF its set
  if( !is.null(m) ) {
    message("gets data from cache")
    return(m)
  }
  
  ## Get matrix to data
  data <- x$get()
  
  ## Calculate inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set inverse to the object
  x$setInverse(m)
  
  ## Return matrix
  m
}