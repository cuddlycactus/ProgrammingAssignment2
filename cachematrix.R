## CACHES THE INVERSE OF A MATRIX TO DUMB DOWN THE PROCESSING REQUIRED

## The first function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL  ##local variable that will contain the matrix is assigned null value
     
     ##Creation of 4 fxns: 
     
     set <- function(y) {   
     ##when matrix is modified, set assigns x to y & m to null outside this function
          x <<-y            
          m <<- NULL
     }
     
     get <- function() x 
     ##just returns original input matrix

     setinverse <- function(solve) m <<- solve
     ##solves inverse of matrix and returns value to external environment

     getinverse <- function() m
     ##just returns the inverse

     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes, caches, and solves the inverse of the matrix

cacheSolve <- function(x) {
     m <- x$getinverse() ##If there's a cached matrix, return it to value m
     if(!is.null(m)) {    ##Obtain previously set value m if it's cached
          message("Getting cached data")
          return(m)      ##Housekeeping? Hashing it out results in no change
     }
     data <- x$get()     ##Obtains the matrix and assigns to local variable data
     m <- solve(data)    ##Returns the inverse of the matrix to value m
     x$setinverse(m)     ##Caches the inverse stored in m by calling previous function
     m                   ##Returns m
}
