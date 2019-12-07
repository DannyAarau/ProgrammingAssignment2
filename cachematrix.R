## creates matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInversedMatrx <- function(solve) m <<- solve
  getInversedMatrix <- function() m
  list(set = set, get = get,
       setInversedMatrx = setInversedMatrx,
       getInversedMatrix = getInversedMatrix)
}

## creats inversed matrix or gets it from the cache

cacheSolve <- function(x, ...) {
  m <- x$getInversedMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInversedMatrix(m)
  m
}
