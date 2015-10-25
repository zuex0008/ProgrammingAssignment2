## makecachematrix function will cache the matrix that is passed to the function 
## the function will return the a list 

makeCacheMatrix <- function(x = matrix())
{
  # initialize inv variable to null
  inv = NULL
  
  # the matrix value is set in the environment
  set = function(y)
  {
    ## the assigned value is passed to the external variable x
    x <<- y
    inv <<- NULL
  }
  
  # will return the matrix set above 
  get = function() x
  
  ## set the inverse value to the variable for future use
  setinv = function(inverse) inv <<- inverse 
  
  ## get the inverse value assigned to the variable
  getinv = function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## cachesolve will find the inverse of the matrix that is passed to the function 
## from makecachematrix variable, if the inverse is not calculated   

cacheSolve <- function(x, ...)
{
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  ## if the inverse has already been calculated
  if (!is.null(inv))
  {
    ## get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  ## get matrix from the makecachematrix get function
  mat.data = x$get()
  ## calculate inverese of the matrix 
  inv = solve(mat.data, ...)
  
  ## sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  ## return the calculated value 
  return(inv)
}
