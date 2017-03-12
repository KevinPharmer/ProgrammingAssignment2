## The two functions allow user to calculate the inverse of
##a matrix and store it in the Cache.  If the same inverse is 
##needed again, the cacheSolve function will retrieve it from the Cache
##rather than recalculating the inverse.

## The first function contains a list of functions to...
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  set <- function(y) {##create matrix in working environment
    x <<- y
    InvMat <<- NULL
  }
  get <- function() x ##get value of matrix
  ##invert matrix and store in cache
  setInverse <- function(inverse) InvMat <<- inverse
  ##get data from cache
  getInverse <- function() InvMat
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve. The function calculates the inverse of the matrix.
##If the inverse has been previously calculated, the function will take the value
##from the cache

cacheSolve <- function(x, ...) {
  InvMat <- x$getInverse()
  if(!is.null(InvMat)) {
    message("getting cached data")
    return(InvMat)
  }
  data <- x$get()
  InvMat <- solve(data, ...)
  x$setInverse(InvMat)
  InvMat
  ## Return a matrix that is the inverse of 'x'
}

