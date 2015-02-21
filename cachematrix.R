## creates the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  #clears the inverse matrix
  inverse_matrix <- NULL
  #sets the value of x (the matrix) and clears the inverse matrix
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  #gets the value of x i.e. the matrix that the programmer enters
  get <- function() {
    x
  }
  #creates the inverse matrix (inverse of x)
  set_inverse <- function(solve){
    inverse_matrix <<- solve
  }
  #calls the inverse matrix
  get_inverse <- function() {
    inverse_matrix
  }
  #shows a list of the above functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## creates an inverse matrix if the get_inverse value is still NULL.
#if get_inverse does have a value, calls that from memory

cacheSolve <- function(x, ...) {
  #sets inverse_matrix to the saved value (if there is one)
  inverse_matrix <- x$get_inverse()
  #checks to see if there is a saved value of inverse matrix 
  #and returns it if so
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  #if no inverse matrix, gets the value of X from the
  #makeCacheMatrixfunction, uses the solve function to invert the matrix
  else {
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    #calls the set inverse command from the makeCacheMatrix function
    #and returns the inverse matrix
    x$set_inverse(inverse_matrix)
    inverse_matrix
  }
}

