## makeCacheMatrxi takes in a matrix. Defines the operations: set, get, getSolution, setSolution
## For subsequent matrices, call set to set a new matrix to cache for. Only Caches the previous one. 

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  
  set <- function(y) {
    x <<- y
    sol <<- NULL
  }
  get <- function() x  
  getSolved <- function() sol
  setSolved <- function(solution) sol <<- solution
  
  list(set = set, get = get, getSolved = getSolved, setSolved = setSolved)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## cacehSolve first compares the input with its cache, if it's the same, return the cahced result
## If it's different, then calls solve, which solves it. 

cacheSolve <- function(x, ...) {
  ##Get the solution. If solution exists, then return it. 
  s <- x$getSolved()
  if(!is.null(s)) {
    message ("Getting the cached inverse")
    return(s)
  }
  
  ##Solution doesn't exist, so grab the actual matrix, solve it and push it back
  mat <- x$get()
  sol <- solve(mat, ...)
  x$setSolved(sol)
  sol  
}
