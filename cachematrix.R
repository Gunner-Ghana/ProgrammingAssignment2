## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
##cacheSolve function computes the inverse of the special of the matrix
##returned by makeCacheMatrix

## The function below creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getmean <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}



##The function below computes the inverse of the special of the matrix
##returned by makeCacheMatrix.if the inverse has already been computed,
## cacheSolve should retrieve the computed inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting the inverse of the matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
