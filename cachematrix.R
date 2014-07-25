## cachematrix calculates and caches the inverse of a matrix.  

## makeCacheMatrix is a class which define 4 functions/methods for use with cacheSolve.
## The matrix object x and its inverse i can be initialised and accessed 
## via these functions.
## i the matrix inverse from cacheSolve is cached here (through setInverse).
## The <<- operator extends the scope of i and x beyond the inner functions
## (set and setInverse) to the parent function makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL ## initialise the new matrix inverse to null
  
  set <- function(y) {
    
    ## assigns a new matrix y to object x
    x <<- y   
    
    ## sets inverse value i to null, as it is not computed yet for this matrix
    i <<- NULL  
  }
  
  ## function to return the matrix for cacheSolve
  get <- function() x
  
  ## function to store/cache the computed matrix inverse 
  ## from cacheSolve ('inverse') in variable i defined above 
  setInverse <- function(inverse) i <<- inverse
  
  ## function to return the matrix inverse
  getInverse <- function() i
  
  ## 4 functions/methods are returned in a list 
  ## These are Setter and Getter methods for x (matrix object)
  ## These allow cacheSolve to access the matrix values in x, check for a 
  ## cached inverse (i !null) and to cache the computed inverse in 
  ## variable i of makeCacheMatrix
  
  list(set = set, get = get,
       setinverse = setInverse,
       getinverse = getInverse)  
    
}


## cacheSolve is a function to return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
 
  ## try to get the cached matrix inverse  
  i <- x$getinverse()
  
  ## if it is already computed (!NULL) for this matrix, return it 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## if this matrix inverse has not been computed then .....
  
  ## get matrix object using makeCacheMatrix class method get()
  data <- x$get()
  
  ## compute matrix inverse
  i <- solve(data, ...)
  
  ## cache the inverse value in makeCacheMatrix 
  x$setinverse(i)
  
  ## return the computed inverse
  i
}
