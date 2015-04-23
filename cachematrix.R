## This file contains a set of functions to work with the cached 
## inverse of  matrix.
##
##    makeCacheMatrix - This function creates cached objects
##    cacheSolve      - Function to get the inverse of matrix 
##                      from cache or from the output of 
##                      "solve" function
##    testCacheSolve  - Additional function to test how cache
##                      improve performance
##    Example of use:
##      source("cachematrix.R")
##      m<-matrix(sample(0:100,1000000,replace=T),1000,1000)
##      testCache(m)
##
##    Example of output from testCacheSolve:
##      Time difference of 1.548089 secs
##      getting cached data
##      Time difference of 0 secs
##

makeCacheMatrix <- function(x = matrix()) {
  ##  @x: generally any object, but in this case class(x) function
  ##    should return "matrix"
  ##  
  ##  Function creates an object which represents a list of functions 
  ##    to manage two internal variables: one variable for input 
  ##    and one variable for output 
  ##    Object and it internal variables holds in memory, 
  ##    in other words cached.
  
  m <- NULL     # nullify internal variable which holds output
  setInVar <- function(y) {  # function to set input variable
    x <<- y     # "x" is internal variable which holds input
    m <<- NULL  # nullify internal variable which holds output
  }
  getInVar <- function() x   # function to return input variable
  setOutVar <- function(cm) m <<- cm   # function to set output variable
  getOutVar <- function() m # functionto return output variable
  list(setin = setInVar, getin = getInVar,    # return list and bind
       setout = setOutVar,                    # names to internal functions
       getout = getOutVar)
}

cacheSolve <- function(x, ...) {
  #   @x:   a special makeCacheMatrix object with methods to manage 
  #         two internal variables which holds square invertible matrix
  #   
  #   Function will return cached reverse matrix if it is available in cache,
  #   otherwise it will run solve function and put output to makeCacheMatrix
  #   object
  m <- x$getout()       # check cached output variable
  if(!is.null(m)) {
    message("getting cached data")
    return(m)           # return cached output variable
  }
  matrix <- x$getin()     # get input matrix
  m <- solve(matrix, ...) # put reverse matrix to variable
  x$setout(m)             # set output variable in makeCacheMatrix
  m
}

testCacheSolve = function(x) {
  ## @x:  a square invertible matrix
  ## Function to test how cache improve performance
  ##
  ## Example:
  ##  m<-matrix(sample(0:100,1000000,replace=T),1000,1000)
  ##  testCache(m)
  ##
  ## Output:  
  ## Time difference of 1.548089 secs
  ## getting cached data
  ## Time difference of 0 secs
  ##
  
  m = makeCacheMatrix(x)  # create object to cache square invertible matrix
  
  start.time = Sys.time()       # get start time
  cacheSolve(m)                 # run optimized function
  d = Sys.time() - start.time   # get end time
  print(d)                      # print duration
  
  start.time = Sys.time()       # get start time
  cacheSolve(m)                 # run optimized function
  d = Sys.time() - start.time   # get end time
  print(d)                      # print duration
}
