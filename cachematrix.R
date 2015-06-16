## These functions are used to create a special object 
## that stores a matrix and caches its inverse.
## For this assignment, it is assumed that the matrix supplied
## is always invertible

## Function makeCacheMatrix creates a special object,
## that allow set and get value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
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

## Function cacheSolve calculates inverse of the matrix,
## stored in the object which was created with makeCacheMatrix.
## Function retrieves inverse from the cache. If inverse has not
## been calculated, it calculates it and writes to cache.
## Then it returns inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(is.null(i)) {
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
    }
    i
}
