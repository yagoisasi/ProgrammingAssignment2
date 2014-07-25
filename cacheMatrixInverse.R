
# makeCacheMatrix creates a special "matrix" object that can cache its inverse
# and offers the following list of functions:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The function cacheSolve calculkates the inverse of the matrix passed as parameter.
# It is calculated for the first time and cached for later calls
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  #Solve computes the inverse assuming it is invertible
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
#Samples
# > x = rbind(c(1, 0), c(0, 1)) 
# > m = makeCacheMatrix(x)
# > inv = cacheSolve(m)
# > inv
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1

# > x = rbind(c(1/2, 0), c(0, 1/2)) 
# > m = makeCacheMatrix(x)
# > inv = cacheSolve(m)
# > inv
#       [,1] [,2]
# [1,]    2    0
# [2,]    0    2

# > x = rbind(c(1, 0.5), c(0.5, 1)) 
# > m = makeCacheMatrix(x)
# > inv = cacheSolve(m)
# > inv
#           [,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.3333333


