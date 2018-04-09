## "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  d <- NULL
  set <- function(y) {
    x <<- y
    d <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) d <<- inverse
  getinverse <- function() d
  list(set = set,get = get,
    setinverse = setinverse,
    getinverse = getinverse)}

## "cacheSolve" function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
  inv <- x$getinverse() 
  if(!is.null(inv)) { 
  message("getting cached data.") 
  return(inv)} 
  data <- x$get() 
  inv <- solve(data) 
  x$setinverse(inv) 
  inv } 

