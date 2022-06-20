## These functions cache the value of a computation
## (inverse of a matrix) so it can be looked up instead
## of being recomputed.
## This first function creates the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve,
             getsolve = getsolve)
}

## This function checks if the inverse was already
## calculated, if yes it retrieves it, else it calculates it

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m) {
              message ("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
