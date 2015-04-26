## Two functions: one to create a special matrix object 
## that can cache its inverse and another to compute the
## inverse of the special matrix. If inverse has already been 
## calculated, then retrieve from cache. 

## Cache the matrix of x

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Computes the inverse of makeCacheMatrix.  If inverse has
## already been calulated, inverse retrieved from cache.

cacheSolve <- function(x=matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}