## functions to cache the inverse of a matrix
## useful to limit computations if subsequent referring to the inverse matrix planned

## makeCacheMatrix => create object for matrix x and its cached inverse along with 'methods' to access them

makeCacheMatrix <- function(x = matrix()) { 
  
  m <- NULL #initialize m with NULL for further storage of the inverse
  
  set <- function(y) { #initialize matrix in inverse in global environment - can be used in directly to set new input matrix
    x <<- y # set input matrix
    m <<- NULL # resets inverse
  }
  
  get <- function() { #returns input matrix
    x
  }
  
  setSolved <- function(Solved) { # stores inverse in global environment
    m <<- Solved
  } 
  
  getSolved <- function() { # returns calculated inverse or null if input matrix not solved yet 
    m
  }
  
  list( # creates a list of functions ('methods') to access created object
    set = set, 
    get = get,
    setSolved = setSolved,
    getSolved = getSolved
  )
  
}


## cacheSolve => Return a matrix that is the inverse of 'x'
##            => if the inverse has already been calculated (and the matrix has not changed), 
##            => then the cacheSolve  retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  
  m <- x$getSolved() # returns calculated inverse or null if input matrix not solved yet 
  
  if(!is.null(m)) { # checks if inverse already calculated and cached
    message("getting cached matrix") # info appears if returned solved matrix 
    return(m) # returns cached inverse and close function
  }
  
  # when no solved and cached matrix yet
  data <- x$get() # returns input matrix to be solved and store in data
  m <- solve(data, ...) # solve matrix and stores inverse in m
  x$setSolved(m) # cache m in global environment
  m # display inverse
}
