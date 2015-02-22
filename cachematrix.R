## below functions build a matrix and calculate its inverse, caching the inverse
## in a variable in the global environment to allow for its retrieval so as to avoid
## intensive recalculations when reusing the inverse matrix

## makeCacheatrix builds a matrix (x) and sets or retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
     

}


## cacheSolve will calculate the inverse of the matrix passed,
## if the matrix has changed or the cache is empty. If the inverse is cached, 
## the cache will be retrieved from the global environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
     
}
