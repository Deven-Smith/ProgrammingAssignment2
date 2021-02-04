## These functions take an invertable matrix, and create it's inverse to be stored in cache

## This function:
  ##sets the value of the matrix
  ##gets the value of the matrix
  ##sets the inverse of the matrix
  ##gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix.
##It checks if the inverse has already been calcualted first.
  ##If so, it gets the inverse matrix from cache
  ##If not, it calculates the inverse of the matrix and stores it in cache via
    ##the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("calculating")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



###code i used to test result:
  ##square <- matrix(data=round(rnorm(9,mean=5,sd=1),digits=0),nrow=3,ncol=3)
  ##test <- makeCacheMatrix(square)
  ##cacheSolve(test)
  ##cacheSolve(test)
  ##cacheSolve(test) == solve(square)


##the first time cacheSolve(test) is called, a message saying "calculating"
  ##appears to show the inverse is being calculated for the first time
##the second time cachSolve(test) is called, a message saying
  ## "getting cached data" appears to show the cached value is being used
##cacheSolve(test) == solve(square) verifies the function result is
  ##equivalent to the manually calculated result














