## 17/11/14 cachematrix.R 

# To test functions
# 
# 1. Create a matix 
# m <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
# 
# 2. Cache the matrix 
# x <- makeCacheMatrix(m)
# 
# 3. Run cacheSolve
# cacheSolve(x)
# 
# 4. Run again to check the cache is used.
# cacheSolve(x)

## Function acts as access to the x matrix.  x is of type list with refrence to functions to 
## manipulate the data held by x.  x is given a global scope
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## A method which uses the variable x to get the orginial matrix and then perform the inverse
## method checks to see if the inverse is already calculated before 
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
