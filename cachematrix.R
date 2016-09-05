## Put comments here that give an overall description of what your
## functions do


## This function creates an environment that allows the user to
## Create a matrix, which inverse can be cached
## Through returning a list of functions 
## That take advantage of lexcical scoping

makeCacheMatrix <- function(x = matrix()) {
   
  # set the value of the matrix
  m <- NULL
  set <- function(y) {
    
    x <<- y
    m <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setInverse <- function(inverse) m <<- inverse
  
  #get the value of the inverse
  getInverse <- function() m

  list ( set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }


## This function will either return the cached value of the inverse of the matrix
## If it has been stored 
## Or it will calculate the inverse of the matrix 
## And cache this value
## It also returns the result of the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  # check to see if the inverse has already been calculated
  
  #if yes gets the inverse from the cache and skips the computation
  
  # if not, calculates the inverse via the setInverse function
  
    m <- x$getInverse()
    if(!is.null(m)) {
      message("Getting cached data")
      return(m)
    }
  
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

makeVector <- function(x = numeric()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x      
  
  setmean <- function(mean) m <<- mean
  
  getmean <- function() m
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}