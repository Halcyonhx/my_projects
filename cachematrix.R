## The pair function are together to cache the inverse of a matrix

## The first function is to creat a list containing:
## 1. set: to set the matrix
## 2. get: to get the setted matrix
## 3. setsolve: to set the inverse matrix
## 4. getsolve: to get setted inverse matrix

makeCacheMatrix <- function( x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
  }
  get <- function()x
  setsolve <- function(solve){
    m <<- solve
  }
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) 
}

## The second function calculates inverse matrix if it has not been setted; or return the inverse matrix setted before

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if (is.null(m)) {
    m <- solve(x$get())
    return(m)
  }
  else {
    print("get cached inverse matrix")
    return(m)
  }
}