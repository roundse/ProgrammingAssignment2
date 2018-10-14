## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # Initialize inverse value to null.
        inv <- NULL
        
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Check if inverse is stored.
        inv <- x$getinverse()
        if (!is.null(inv)) {
            message("Retrieving cached inverse of input matrix.")
            inv <- x$getinverse()
            return(inv)
        }
        data <- x$get()
        print(data)
        inv <- solve(data)
        x$setinverse(inv)
}

