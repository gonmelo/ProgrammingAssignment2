## The first function makeCacheMatrix creates a list containing with a matrix and
## 4 functions necessary to manipulate its value and the value of its inverse. The
## second one allows to calculate the inverse of the referred matrix, skipping the
## calculations if the value is already stored in cache.

## Function that creates a R object (list) that stores a matrix and its inverse
## as 2 data objects (x and i) along with 4 functions necessary to: set a matrix,
## get the value of the matrix, set its inverse, get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse 
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function that calculates the inverse of the matrix x taken as argument in the
## function makeCacheMatrix. It first checks if the inverse as been calculated 
## before. If so it gets the value from the cache, skipping the calculation. If 
## not, then it calculates it and sets it as the inverse using the setinverse 
## function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
