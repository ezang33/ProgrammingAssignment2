## First function is makeCacheMatrix which creates an object to store a matrix 
## (x) and its inverse (m). The second function is cacheSolve which uses the 
## makeCacheMatrix object to either create or retrieve its inverse.

## The makeCacheMatrix function first initializes objects x and m. Then it
## defines the functions that make up the makeCacheMatrix object. These
## functions are for getting matrix x and inverse m, or setting matrix x 
## and inverse m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function utilizes the functions of the makeCacheMatrix object
## to first check if an inverse was already cached.If no inverse has been cached
## then the function will utilize the solve() function to set the inverse. Then
## subsequent calls of the cacheSolve function on the same makeCacheMatrix
## object will retrieve the cached inverse instead of solving for it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting the Cached Inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
