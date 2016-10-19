## Creates a list object that can contain cache inverse matrix
makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x
  
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  
  ## Method to get the inverse of the matrix
  getInverse <- function() inv
  
  
  ## Return a list of the previous 4 methods
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}

## Compute the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated (and the matrix is the same ),
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  ## Just return the inverse if its already cached
  if( !is.null(inverseMatrix ) ) {
    message("getting cached inverse matrix")
    return(inverseMatrix )
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inverseMatrix  <- solve(data)
  
  ## Set the inverse to the object for a future request
  x$setInverse(inverseMatrix )
  
  ## Return the matrix
  inverseMatrix 
}