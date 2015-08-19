##The first primary function is a compound function which returns a list of four elements.
##Subfunction1: input a newmatrix and reset the cached value to NULL. 
##Subfunction 2: return the current input matrix.
##Subfunction3 caches the computed value from the second primary function.
##Subfunction4 returns the cached i value.
##The second primary function ultimately calculates the inverse of the entered matrix. It first checks for a
##cached value and returns that value if needed. It then calculates the inverse of the entered matrix

makeCacheMatrix <- function(x = matrix()) {    ##Define function with numeric argument
  i <- NULL                                     ##Create variable i in which to store the return of "cacheSolve"
  set <- function(y) {                          ##Define function which allows user to use different data
    x <<- y                                     ##which resets x to y
    i <<- NULL                                  ##and i to null
  }
  get <- function() x                           ##Define fuction which returns x
  setinverse <- function(inverse) i <<- inverse ##Define function which caches last value of i calculated
  getinverse <- function() i                    ##Define function which returns value of cached i
  list(set = set, get = get,                    ##Return a list of 4 elements corresponding to the above functions
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {                ##Define a function with input as the return from "makeCacheMatrix"
  i <- x$getinverse()                           ##Create a variable i which retrieves x$getinverse
  if(!is.null(i)) {                             ##Create a logical to test if i is NULL
    message("getting cached data")
    return(i)                                   ##Return i
  }
  data <- x$get()                               ##Input data from makeCacheMatrix
  i <- solve(data, ...)                         ##Apply solve() to matrix to calculate the inverse matrix
  x$setinverse(i)                               ##Update the cache with new matrix
  i                                             ##Return the new value for the matrix inverse
}

mat2 <- matrix(1:4, nrow = 2, ncol = 2)
mat2

a <- makeCacheMatrix(mat2)
a

cacheSolve(a)