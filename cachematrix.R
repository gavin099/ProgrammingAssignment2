# The function would first record the inverse of the input matrix,
# store it in cache, then output its value in cacheSolve function.

# This is the first step of the whole function: calculates the inverse 
# and stores its value
makeCacheMatrix <- function(x = matrix()) {
  if(ncol(x) == nrow(x) && det(x)!= 0){
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)}
  else{
    message("The matrix you've put is singular.")
  }
}


## Write a short comment describing this function
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data, ...)
  x$setinverse(m)
  m    
}
