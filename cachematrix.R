

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)inver <<- inverse
  getInverse <- function() inver
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
  
}


## This functin computes the inverse of the matrix created by 
## makeCaheMatrix above.if the inverse has already been calculated, 
## it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix <- x$get()
  inver <- solve(matrix, ...)
  x$setInverse(inver)
  inver
}
