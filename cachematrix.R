## Put comments here that give an overall description of what your
## functions do
## The two functions cache the inverse of a matrix.

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse. It is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)iv <<- solve
  getinverse <- function()iv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## It first checks to see if the inverse matrix has already been calculated 
## (and the matrix has not changed). 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the 
## value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
