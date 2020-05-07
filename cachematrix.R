## Put comments here that give an overall description of what your
## functions do

## Takes a square matrix that can be inverted and returns
## a list of functions to set the matrix, 
## invert the matrix, 
## set the inverse,
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, 
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}



## Write a short comment describing this function
## Takes the output of makeCacheMatrix() and returns the inverted matrix
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