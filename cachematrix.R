## makeCacheMatrix defines the set of functions that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the inverse of the matrix
## 4. gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y = matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve can:
## 1. retrieve from cache the inverse of 'x' (if it's already calculated)
## 2. calculate the inverse of 'x' and store it in the cache of 'x'

cacheSolve <- function(x, ...) {
    
    i <- x$getinv()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
