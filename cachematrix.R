## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Method to get the matrix
        get <- function() x
        
        ## Method to set the inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## Method to get the inverse matrix
        getinverse <- function() m
        
        ## Populate list of functions neede
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}



## Finds the inverted matrix either through solve or retrieves it from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## If the inverse is found in the cache returns it from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Gets the matrix
        data <- x$get()
        
        ## Get the inverse of the matrix using solve()
        m <- solve(data, ...)
        
        ## Sets the inverse of the matrix to m
        x$setinverse(m)
        
        # Returns inverted matrix
        m
}