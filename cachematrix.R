## the following 2 functions are working together
## the idea is to save time of calculating the inverse of the matrix
## this is done by calculating the inverse only when
##            1) a new cached matrix is created by calling makeCacheMatrix
##            2) when the inner matrix value is changed by calling makeCacheMatrix$set
##     otherwise, the saved calculated inverse is retrieved
## to know if a new calculation is needed, m is set to null at first call of makeCacheMatrix and when set is called


## this function takes a matrix as input and returns a list of functions
## once it's called, an object is created holding a matrix
## 2 functions setter and getter of the matrix are created.
## 2 functions setter and getter for inverse are created.
## ( m ) is set to null at first line and in set function to indicate that no cached inverse existing
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        # optional if statement condition , checking if the setter is called with a really new matrix or no
        # so if it's called with the same saved matrix, no need to calculate the inverse again
        # this is useful if high probability of calling with same matrix, otherwise it should be removed to reduce 
        # processing headache
        if(!identical(x,y)) 
        {
            x <<- y
            m <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## this function checks if the inverse of the cached matrix is existing or not
## if it exists, it returns it directly
## if not, it calculated it, and save it with setinverse, so that it won't be calculated again next time
##      ** there is assumption that the matrix is invertible, so checking that is skipped ***
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    # if the inverse is calculated  before
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if inverse wasn't calculated before, calculate it and cache it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
