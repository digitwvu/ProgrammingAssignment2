## Functions created for Programming Assignment 2
## functions act together to cache the inverse of a matrix

## Creates an object that stores a matrix and the cache's the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   #set initial value of inverse
  set <- function(y) {                  #can be called to reset the values in the object
    x <<- y
    i <<- NULL
  }
  get <- function() x                   #returns the matix
  setinv <- function(inv) i <<- inv     #stores the inverse of the matrix
  getinv <- function() i                #returns the saved inverse
  list(set = set, get = get             #maintains a function list
       setinv = setinv,
       getinv = getinv)
  
}


## Function checks to see if a cached value for the inverse exists
## if not it calculates and saves the value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()                       #get the value of cached inverse
  if(!is.null(i)) {                     # check if cached inverse has a value
    message("getting cached data")      # and return if so
    return(i)
  }
  data <- x$get()                       #if no value cacched get the stored matrix
  i <- solve(data, ...)                 #calculate the inverse
  x$setinv(i)                           #store the inverse
  i                                     # return the inverse
  
}
