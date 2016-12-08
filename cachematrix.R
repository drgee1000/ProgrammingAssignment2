## makeCacheMatrix - constructs a CacheMatrix object that can store its inverse to avoid recalculation
## cacheSolve - method to calculate and return a cached inverse of a CacheMatrix object.

## Constructs a CacheMatrix which is an object (list) that contains matrix data, an inverse attribute,
## and functions to get and set the matrix and get and set the inverse

makeCacheMatrix <- function(x =matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Apply solve to calculate the inverse of a special cachable matrix x. 
## Returns a previously cached inverse if exists in x or calculates an inverse and saves in x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
