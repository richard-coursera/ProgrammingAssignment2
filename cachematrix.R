## These two functions make a matrix inverse and cached.
##
## makeCacheMatrix creates the four operations on a matrix. 
## 1. Set up the matrix
## 2. Get the matrix
## 3. Set up the inverse matrix
## 4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve function checks whether the inverse matrix is cached.
## If inverse matrix is cached, it will be returned.
## Otherwise, cacheSolve makes the matrix inverse, and inverse matrix is cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
