makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  setMat <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inverse) Inv <<- Inverse
  getInv <- function() Inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setmean(Inv)
  Inv
}
