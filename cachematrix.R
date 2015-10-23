## Caches the inverse of a matrix to dumb down the processing required


## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<-y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes or solves the inverse of the matrix

cacheSolve <- function(x) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("Getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
