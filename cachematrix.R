## Solution to Coursera R Programming Programming Assigment 2
## 
## Set of functions to illustrate the use of the '<<-' operator,
## which causes a search to be made through parent environments for 
## an existing definition of the variable being assigned. If such 
## a variable is found (and its binding is not locked) then its 
## value is redefined, otherwise assignment takes place in the 
## global environment.

## The makeCacheMatrix function returns a list of 4 elements, all
## of them functions. They are used to get and set the internal
## representation of the square matrix to be inverted; as well as
## for getting and setting the inverse matrix itself.

makeCacheMatrix = function(x = matrix()) {
      inverse = NULL
      set = function(y) {
            x <<- y
            inverse <<- NULL
      }
      get = function() x
      
      setInverse = function(valueInverse) inverse <<- valueInverse
      
      getInverse = function() inverse
      list(set = set, get = get, 
           setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function returns the inverse matrix of the square,
## invertible matrix stored in the cache matrix produced by the function
## makeCacheMatrix. If the inverse matrix was already calculated and 
## the base matrix was not modified, a cached result is return. If that
## is not the case, the inverse matrix is calculated, cached and returned.

cacheSolve = function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inverse = x$getInverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setInverse(inverse)
      inverse
}
