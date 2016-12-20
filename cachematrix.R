#####################################################
#                                                   #
#     Coursera - R Programming Week 3 Assignment    #
#          Submitted by: Christopher Zuber          #
#                                                   #
#####################################################

## The two functions below cache the inverse of a matrix

## Function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse  
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) inv <<- solveMatrix
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv) ){
    message( "getting cached data" )
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}