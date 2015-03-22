## The following functions check if the inverse of a matrix is available at the cache, and otherwise they calculate it 

## This function creates a list with functions to manage the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function (inv) i <<- inv
  getinv <- function () i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of a matrix, but checks first if it is available at the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
