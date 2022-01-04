## This pair of functions caches the inverse of a given invertible matrix



# makeCacheMatrix: This function creates a special "matrix" object which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the inverse of the matrix
# get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(Y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) M <<- solve
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# However, it first checks whether the inverse has already been calculated (and the matrix has not changed). 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise it calculates the inverse of the data, 
# and set the value of the inverse in the cache via the setinverse function.



cacheSolve <- function(x, ...) {
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setinverse(M)
  M
}


x <- matrix(c(1,2,3,4),2,2)

cacheSolve(makeCacheMatrix(x))



