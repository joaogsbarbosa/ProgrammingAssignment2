## The following functions work together to create an inverse matrix and
## make the inverse of the matrix available in the cache environment

## Create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  cache <- NULL
  
  get <- function() {
    x
  }
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  setMatrix <- function(inverse) {
    cache <<- inverse
  }

  getInverse <- function() {
    cache
  }
  
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## Calculates the inverse of the matrix if the inverted
## matrix does not exist in cache
## Else get the matrix in cache
cacheSolve <- function(x, ...) {
  
  inverseCache <- x$getInverse()
  
  if (!is.null(inverseCache)) {
    message("getting cached data")
    
    return(inverseCache)
  }
  
  matrix <- x$get()
  
  inverseCache <- solve(matrix, ...)
  
  x$setMatrix(inverseCache)
  
  inverseCache
}
