##There are 2 functions here: makeCacheMatrix takes a matrix as an input and returns a list having 4 functions.
##cacheSolve takes the list of functions as an input. It calculates the inverse of the matrix



## The function makeCacheMatrix takes a matrix as an input. Inside this function,
## other functions are defined. They are: set - to set the value of the matrix, get - to get the value of the matrix
## setinverse - to set the inverse of the matrix, getinverse - to get the value of the inverse. The function makeCacheMatrix
## does not calculate the inverse of a matrix. It just sets and gets the value of a matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##The function cacheSolve takes the list of functions as an input. 
##It checks if any value is set for inverse. If yes, then it returns that value. 
##If no value is set, then it calculates the inverse of the matrix and sets that 
##value as the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cache data")
    return (m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
