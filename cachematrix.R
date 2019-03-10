#makeCacheMatrix is a function that contains four functions which are getters and setters
#The getters will retrieve the data and the setters will set the data values within an object
#The <<- operator is used to assign a value to an object in an environment that is different
#from the current environment
#Each of the four functions are put into a list,with elements of the same name as their funcitons

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve will place the value of the getInverse funciton into the local variable inverse
##Then the function checks to see if there is a value for inverse, 
##if there is a value, it will be returned
##If there is no value for inverse, then the function will obtain the data from the 
##makeCacheMatrix function and place it into the local variable data
##The function will then take the inverse of the data and store it in the parent environment

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <-x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}