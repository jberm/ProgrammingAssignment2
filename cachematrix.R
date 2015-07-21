# Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

# Computes the inverse of a matrix that was created
# using the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m = x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
