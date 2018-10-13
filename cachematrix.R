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
        setinverse <- function(inverse) inv <<- solve()
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # Check if inverse is stored.
        if (!x$getinverse) {
                ## Check to see if x is a matrix and if yes,
                ## return a matrix that is the inverse of 'x'
                if (is.matrix(x)) {
                        inv <- solve(x)
                }
        }
                
}
