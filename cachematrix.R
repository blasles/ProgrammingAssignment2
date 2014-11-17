## These functions caches the result of a invere computation of a matrix


## makeCacheMatrix takes in a matrix and creates a list that is read by function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
## Sets a new Matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
## Sets inverse of Matrix
  setInv <- function(Inv) m <<- Inv

## Gets inverse of Matrix
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}

## cacheSolve takes in makeCacheMatrix and solves the inverse maxtrix in the function. 
## cacheSolve returns a cache matrix if computation has been previously executed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
