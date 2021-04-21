# Two functions to calculate or catch the inverse of a matrix 

# First function to build and return a list of functions to set and get a matrix

makeCatchMatrix <- function(x = matrix()) {
  # initiate a matrix and object m to get the inverse matrix in the later code
   m <- NULL
  # assign the input of x and value of m to the parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get x from the parent environment
  get <- function() x
  # assign invert matrix value to m in parent environment
  set_inv_matrix <- function(inv_matrix) m <<- inv_matrix
  # retrieve the value of the invert matrix
  get_inv_matrix <- function() m
  # return the list of 4 functions with each element named
  list(set = set, get = get,
       set_inv_matrix = set_inv_matrix,
       get_inv_matrix = get_inv_matrix)
}

# 2nd function to calculate or retrieve the invert matrix of the input from makeCacheMatrix()
cachesolve <- function(x, ...) {
  # retrieve the invert matrix from the cache
  m <- x$get_inv_matrix()
  # if not NULL (no new matrix input), return the cached invert matrix to the parent environment
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if it's a new matrix input, then get, calculate, and return the invert matrix to the parent environment 
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv_matrix(m)
  m
}
