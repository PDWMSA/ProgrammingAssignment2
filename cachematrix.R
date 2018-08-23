
# The first function, makeCacheMatrix creates a special matrix, containing a function to:
# 
# 1)set the value of the matrix
# 2)get the value of the matrix
# 3)set the value of the inverse
# 4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

# The below function calculates the inverse of the matrix created with the function above after
# checking to see if the inverse has already been calculated. If it has, it retrieves the inverse
#from the cache and skips the computation. Otherwise it calculates the inverse of th data. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#The below 3 lines are included simply for ease of testing the above code.
testmatrix <- matrix(c(4, 16, 20, 9 ), 2, 2)
testmatrix2 <- makeCacheMatrix(testmatrix)
cacheSolve(testmatrix2)

#Verifies the math: should return an idenity matrix 
cacheSolve(testmatrix2)%*%testmatrix