## Two functions to calculate the inverse of a matrix using the cache

makeCacheMatrix <- function(x = matrix()) {
  ## Store the inverse of 'x' in the cache
  mat <- NULL
  
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of the inverse
  setinverse <- function(solve) mat <<- solve
  
  ## Get the value of the inverse
  getinverse <- function() mat
  list(set = set, get = get, 
       getinverse = getinverse, 
       setinverse = setinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinverse()

  ## If inverse is already cached, retrieve the inverse from the cache
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  ## Otherwise, calculate the inverse now
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}