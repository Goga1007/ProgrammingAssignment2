makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x)==nrow(x) && det(x)!=0) 
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  
  inv = x$getInverse()
  
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data)
  x$setInverse()
  
  return(inv)
}

x <- makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
cacheSolve(x)

