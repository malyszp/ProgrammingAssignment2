## Pawel Malysz March 2021

## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## default NULL
  set <- function(y) {
    x <<- y  #set matrix value
    m <<- NULL  
  }
  get <- function() x ## get function to get matrix that will be inverted
  setinv <- function(solve) m <<- solve ## Set cached inverse
  getinv <- function() m    ## return cached inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()  ## get cached inverse
  if(!is.null(m)) { ## checks if cached inverse exists
    message("getting cached data")
    return(m)
  }
  data <- x$get()  ## load matrix that needs to be inverted
  m <- solve(data, ...)  ## invert matrix
  x$setinv(m) ## set cached inverse matrix
  m
}

### usage: functions need to be first loaded in environment
### > A<-makeCacheMatrix() 
### > A$set(matrix(c(1,0,0,2),2,2))
### > cacheSolve(A)
###
### run again to see cached inverse is loaded.
### > cacheSolve(A)
