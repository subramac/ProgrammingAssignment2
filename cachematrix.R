## The overall objective of the functions defined here are to understand and test lexical scoping

## The following function makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) ## this function accepts a square matrix as an argument
{
 m <- NULL
 set <- function(y)
  {
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

## The following function cacheSolve computes the inverse of the 
##special "matrix" returned by makeCacheMatrix function created above. 
##If the inverse has already been calculated and the matrix has not changed, 
##then the cachesolve will retrieve the inverse from teh cache instead of 
##recomputing.


cacheSolve <- function(x, ...) 
{
 m <- x$getinverse()
 if(!is.null(m)) ##checking if cache is available for the matrix
 {
   message("Getting cached data")
   return(m)
 }
 data <- x$get()
 m <- solve(data, ...) ## the solve() function computes inverse of a square matrix
 x$setinverse(m)
 m
}
