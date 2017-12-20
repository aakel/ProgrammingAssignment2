
## The following function (makeCacheMatrix) SETS the value of the matrix and the value of its invers
## as well as GETS the value and its inverse

  makeCacheMatrix <- function(x = matrix()) {     ## Initalizing of two objects x and inv
    inv <- NULL                                 ## NULL is a placeholder for the inverse
    set <- function(y) {         
     x <<- y               ## The set function assign the input argument to the x objest in the parent environment        
      inv <<- NULL         ## and assign the value of NULL to the inv object in the parent environment
    }
  get <- function() x     ## The get function takes advantage of the lexical scoping features in R
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }

# The following function returns the inverse of the matrix created by the above makeCacheMatrix function
# assuming that the matrix is always invertible.

  cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
   }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
