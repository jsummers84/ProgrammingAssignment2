##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x, ...){
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Testing the solution:
A <- matrix(c(1,2,3,4),2,2)
> A1 <- makeCacheMatrix(A)
> cacheSolve(A1)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(A1)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
##cacheSolve was able to save the inversion of the matrix without having to repeat the computation