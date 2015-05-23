##This program consists a pair of functions that help to reduce the conputation time to compute the inverse of a matrix. It does so by caching the 
##inverse of the matrix. 

## The function makeCacheMatrix takes the data or the matrix from the user and generates a special "matrix" which can be used to cache the inverse of
## of the matrix. This special "matrix" object is a list containing functions to set the value of the matrix, get the value of the matrix, set the value
## of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {   ## x which is a matrix is the argument of makeCacheMatrix
  r <- NULL                                   ## r is set to null which will later be used to solve inverse matrix
  set <- function(y) {                        ## set function takes a matrix y as its input and set its value to x and sets r back to null
    x <<- y
    r <<- NULL
  }
  get <- function() x                         ## get function does nothing but return the matrix stored in x.
  setinverse <- function(solve) r <<- solve   ## setinverse function sets r to the function solve
  getinverse <- function() r                  ## getinverse function returns the value stored in r
  list(set = set, get = get,                  
       setinverse = setinverse,               ## This is the list of functions that makeCacheMatrix returns when being called.
       getinverse = getinverse)
}


## the function cacheSolve takes the special "matrix" object or the list that makeCacheMatrix makes and returns the cached inverse value, if there is any
## If there is no cached inverse value, it computes the inverse and calls setinverse() function to cache this value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                         ## sets the cached inverse value of x to m
  if(!is.null(m)) {                            
    message("getting cached data")            ## if there is a cached value then it returns that value along with the message "getting cached data"
    return(m)
  }
  data <- x$get()                             ## if there isn't a cached value then it gets the data or matrix from get() function and 
  m <- solve(data, ...)                       ## solves the matrix 
  x$setinverse(m)                             ## and caches the inverse matrix
  
  m                                           ## Return a matrix that is the inverse of 'x'
}
