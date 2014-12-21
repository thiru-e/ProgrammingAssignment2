
### Programming Assignment 2
## This function will create a special matrix that can cache its inverse 

makeCacheMatrix <- function(x=Matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
    }
  
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)

  }

## This function will calculate the inverse of the matrix

cacheSolve <- function(x, ...) {

  inv <- getInv(x)
  
  if (!is.null(inv)) {
    message("Getting cache data")
    return(inv)
    }
  
  data <- get(x)
  inv <- solve(x)
  setInv(inv)
  inv  ## Return a matrix that is the inverse of 'x'
  
  }