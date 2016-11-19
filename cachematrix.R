#The following pair of functions cache the inverse of a matrix 
#for easier computation, rather than computing it repeatedly

#This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ts <- NULL
  set <- function(y) {
    x <<- y
    ts <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) ts <<- solve
  getinverse <- function() ts
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
matrix$get()
matrix$getinverse()

#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve retrieves
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  ts <- x$getinverse()
  if(!is.null(ts)) {
    message("retieving data")
    return(ts)
  }
  data <- x$get()
  ts <- solve(data, ...)
  x$setinverse(ts)
  ts
}

cacheSolve(matrix)
matrix$getinverse()
