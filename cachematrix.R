## Put comments here that give an overall description of what your
## functions do
#  The makeCacheMatrix function takes a single argument of datatype 'matrix' and out of it creates a special matrix, 
#  on which four functions are defined, assinged to variable 'x'.
#   set: assigns 'x' in makeCacheMatrix scope to the value of the input argument, 'y'.  Sets 'm' in makeCacheMatrix
#    scope to NULL.
#   get:  Returns the original matrix 'x'.
#   setinverse:  Assigns 'm' in makeCacheMatrix scope the value of the input argument 'inverse'.
#   getinverse:  Returns the vale of 'm' (in makeCacheMatrix scope), which is the inverse of 'x'.
#
#  cacheSolve function returns either a pre-computed (cached) value of the inverse of 'x', 'm', or it computers
#  the value of 'm' and stores it (in 'm') by calling the 'getinverse' and 'setinverse' functions on 'x'
#  which are described above.

## makeCacheMatrix takes a matrix 'x' as an input argument, sets the inverse of 'x', 'm' to null
## and returns a special 'matrix' object on which four functions are defined.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function( y )
  {
    x <<- y
    # matrix 'x' has been replaced, so set the inverse to NULL
    m <<- NULL
  }
  
  get <- function()
  {
    # return the matrix 'x'
    x
  }
  setinverse <- function( inverse )
  {
    #set the value of the inverse, 'm'
    m <<- inverse
  }
  
  getinverse <- function()
  {
    # return the inverse, 'm'
    m
  }
  
  list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a special 'matrix' (created by makeCacheMatrix), checks if the inverse of 'x' has already been
## computed, and returns it if has.  Otherwise, it calls the 'get' function on 'x', computes the inverse, which it then
## caches, and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  # return the inverse, store in 'm'
  m <- x$getinverse()
  
  if( !is.null( m ) )
  {
    message("Getting cached data...")
    return( m )
  }
  
  data <- x$get()
  m <- solve( data, ... )
  x$setinverse( m )
  m
}
