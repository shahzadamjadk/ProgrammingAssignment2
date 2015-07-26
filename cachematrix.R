makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    
    # use <<- to assign a value to an object in an environment 
    # different from current. 
    
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # calculate the inverse using solve (a, b...)
  # args of solve not specified, returns the inverse
  
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmatrix()
    # if the inverse of 'x' has already been calculated
    
    if(!is.null(m)) {
      # get it from the cache and skip computation
      message("getting cached data")
      return(m)
    }
    
    # otherwise calculate the inverse 
    
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}