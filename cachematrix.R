## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #set initial value to NULL
  tempCache <- NULL
  
  #store the matrix
  setM <- function(y){
    x <<- y
    tempCache <<- NULL
  }
  
  #get the stored matrix
  getM <- function() x
  
  #set the inverse of a matrix
  setInverse <- function(inverse) tempCache <<- inverse
  
  #get the inverse of a matrix
  getInverse <- function() tempCache
  
  #return a list
  list(setM = setM,
       getM = getM,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get the cached value
  tempCache <- x$getInverse()
  
  #return if cached value exist
  if (!is.null(tempCache)){
    message("getting cached data")
    return(tempCache)
  }
  
  #else, calculate the inverse and store it in the cache
  mat <- x$get()
  tempCache <- solve(mat, ...)
  x$setInverse(tempCache)
  
  #return the inverse
  tempCache
}
