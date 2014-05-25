## Program Summary:
##In this program, we compute and store inverse of a square matrix in cache so that when we have
## compute the inverse again, we can look up its value in cache if it exists rather than having 
##to compute it all over again. The first function, makeCacheMatrix() would set value of matrix
## and its inverse to variables that are in different environments than the one in which the 
##function is defined. It also has functions to get the matrix and its inverse. The second function, 
##cacheSolve(), takes matrix as its formal argument and tries to look up value of its inverse (from
##makeCacheMatrix() function). If it does not exist, then, the function computes the inverse by 
##calling inverse function from within makeCacheMatrix() function


## Function Summary:
##makeCacheMatrix function sets value of its formal argument from environment of set function
## and returns it, then sets value of inv variable (which is makeCacheMatrix() local variable) 
##from environment of setInverse() function. Further, it has a get function to return the inverse 
##of matrix. In the end, the function returns a list of set and get values for matrix and its 
##inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function (inverse) inv <<- inverse
  
  getInverse <- function () inv
  
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function Summary:
##Cachesolve() matrix first tries to lookup inverse of given matrix (matrix that is passed 
##to the formal argument x of cacheSolve() function.) from makeCacheMatrix() function. If it gets
##a valid value, it returns the inverse value, else it gets the matrix by calling get from 
##makeCacheMatrix() and then, computes the inverse using solve() function on that matrix. It updates
##the cache by setting / passing the computed inverse value to setInverse variable of 
##makeCacheMatrix() function. In the end, it returns the inverse of matrix (either a looked up value
## or computed value)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse
  
  if (!is.null(inv)){
    
    message ("getting cached data")
    return (inv)
    
  }
  
  mat <- x$get
  inv <- solve(mat)
  x$setInverse (inv)
  inv
}
