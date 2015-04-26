## makeCacheMatrix and cacheSolve functions together enable caching of the inverse of a matrix assumed to be inversible 

## makeCacheMatrix defines list of four functions needed to create the "special" cacheable matrix 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) i <<- Inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve returns a matrix that is the inverse of 'x',returns cached copy if available, else calculates the inverse and stores it for future access 

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
