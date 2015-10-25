## The following codes aims to simplify the computation of an inverse of a function
## It allows you to cache the inverse of a matrix instead of computing it repeatedly

## Creates a special "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
##Creating empty matrix
  inv <- NULL
  ##Creating a base function and setting the value of the matrix
  set <- function (y) {
    x <<- y
    inv<<-NULL
  }
  ##Get the value of  the matrix
  get<-function () x
  ##Set the value of the matrix
  setmatrix<-function(solve)
    inv<<-solve
  ##Get the value of the matrix
  getmatrix<-function() inv
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

## cacheSolve computes the inverse of the special matrix.  It first checks if it is already calculated
## If so, the function will retrieve the inverse from the cached data

cacheSolve <- function(x, ...) {
        inv <- x$getmatrix()
  ##Check if the inverse has been calculated
  ##Get inverse from cached data if so
  if(!is.null(inv)){
    message("getting cached data")
  
  }
  ##Calculate inverse
  matrix <-x$get()
  inv<- solve(matrix, ...)
  ##Set inverse in the cache using the setinverse function
  x$setmatrix(inv)
  inv
}
