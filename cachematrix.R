##  The following functions allow the inverse
##  of a matrix to be stored for quick retrieval
##  thus avoiding the expensive computational effort 
##  of calculating the inverse each time it is needed. 



## The makeCacheMatrix function takes a matrix and
## returns a list of methods that can be used to
## define and display the matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
        
    ## The 'set' method takes the provided matrix argument
    ## and updates the value of the original matrix 
    set <- function(y)  {
      x <<- y
      inv <<- NULL  }

    ## The 'get' method returns the current value of the matrix
    get <- function() x

    ## The 'setinv' method updates the matrix inverse with
    ## the provided matrix argument
    setinv <- function(inverse) inv <<- inverse

    ## The 'getinv' method returns the current value of the inverse
    getinv <- function() inv

    
    list(set=set, get=get, setinv=setinv, getinv=getinv)  }


## The cacheSolve function takes a matrix and returns
## the inverse of the matrix either by calculating 
## it or retrieving it from cache

cacheSolve <- function(x, ...) {

    ## The following statements query cache for the inverse.
    ## If found, the function will return the stored value.

    inv <- x$getinv()
    if(!is.null(inv))  {
        message("getting cached data")
        return(inv)  }

    ## If a value for the inverse is not in cache, the matrix
    ## is retrieved and the inverse calculated and returned.

    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv  }


