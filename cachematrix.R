## The makeCacheMatrix function stores a copy of an input matrix and creates a
## group of functions for storing and retrieving a computation performed on the
## input. The cacheSolve function takes the output of makeCacheMatrix and
## checks to see if the result of the desired computation has already been
## stored previously. If it has, the result is retrieved and returned as the
## current output; otherwise, the computation is performed and subsequently
## stored.

## makeCacheMatrix takes a square and invertible matrix as input, then
## initializes a set of variables and functions for storing and retrieving
## a desired computation. Its output is a list of functions that are defined
## inside of makeCacheMatrix and their definitions (set, for copying the input
## matrix and initializing the inverse; get, for retrieving the input matrix x;
## setinverse for storing the result of a computed inverse; and get, for 
## returning the stored inverse.)

makeCacheMatrix <- function(x = matrix()) {
        # Initialize inverse value to null.
        inv <- NULL
        
        # Store a copy of the input x and initialize the value of the inverse.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Define the function 'get' that retrieves the input x.
        get <- function() x
        # Define a function for storing a computed inverse value.
        setinverse <- function(inverse) inv <<- inverse
        # Define a function for retrieving a stored inverse value.
        getinverse <- function() inv
        # Return a list of the four functions that have just been defined.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes as input the list of functions defined in makeCacheMatrix
## and uses them to see if a result has already been stored for a computation
## performed on a matrix. Its output is the computed inverse on a matrix (stored
## by makeCacheMatrix).
## If the computation has been performed previously, it retrieves
## the stored value of that computation; otherwise, the computation is performed
## and stored.

cacheSolve <- function(x, ...) {
        # Retrieve the stored inverse value.
        inv <- x$getinverse()
        
        # If the stored inverse is not null, return it.
        if (!is.null(inv)) {
                message("Retrieving cached inverse of input matrix.")
                return(inv)
        }
        # If the stored inverse value is null, compute the inverse.
        # First, retrieve the original input matrix.
        data <- x$get()
        # Solve the matrix.
        inv <- solve(data)
        # Store the inverse.
        x$setinverse(inv)
}

