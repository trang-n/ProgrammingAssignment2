## The makeCacheMatrix function creates a cache matrix that can be set and get.
## The cacheSolve function inverts the matrix if it's not cached. If it's cached, it retrieves the
##cached matrix and returns it

## #In this function (makeCacheMatrix) I am setting and getting the matrix that is to be inverted

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  list(set = set,
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## In this function (cacheSolve) the actual inversion of 
#the matirx happens incase it's not already 
#cached. And if it is cached, the cached matrix is just 
#retrieved and returned without having to
#do any inversion calculation. If the matirx isn't cached, 
#after calculating the inverse, the inverted
#matrix is cached for future calculations.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
    
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  
      ## Return a matrix that is the inverse of 'x'
}
