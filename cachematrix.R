## makeCacheMatrix() creates a list containing functions to 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) i<- inverse
    getInverse <- function() i
    list( set = set, get = get, 
          setInverse = setInverse,
          getInverse = getInverse)
}


##cacheSolve() calculates the inverse of the speciallist created 
##with the above function. However, it first checks to see if the 
##inverse has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. Otherwise, it calculates
##the inverse of the matrix and sets the inverse in the cache via the 
##setInverse() function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
      message("getting cached calculation")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
