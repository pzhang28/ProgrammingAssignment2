## The following pair of function return an inverse of matrix. If the inverse is already in the cache, these functions 
## retrieve the inverse from the cache directly without re-calculation.

## makeCacheMatrix takes in an invertable matrix as argument, returns an object (a list) containing four functions.
## makeCacheMatrix also includes two data objects, matrix, x and its inverse, i_m, whose values can be reset or retrived by the four functions.
## Based on lexical scoping rules, get() and set() are used to get or reset 'x'. 
## Likewise, getinverse() and setinverse() are used to retrieve or reset the inverse of 'i_m'.

makeCacheMatrix <- function(x = matrix()) {
  i_m <-NULL
  set <- function (y) {
    x<<-y
    i_m <<-NULL
  }
  
  get <-function () x
  setinverse <-function(inv) i_m <<- inv
  getinverse <- function() i_m
  list (set = set, get = get, setinverse = setinverse, getinverse =getinverse)

}


## cacheSolve computes the inverse of a matrix 'x' using solve. It takes the special object created by makeCacheMatrix
## and retrieves the inverse 'i_m' by x$getinverse(). If 'i_m' is not computed, cacheSolve computes it using solve() 
## and stores it in cache by calling x$setinverse(). If 'i_m' is already in cache, cacheSolve returns its value directly.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i_m <-x$getinverse()
  if(!is.null(i_m)){
    message("getting cached data")
    return(i_m)
  }
  data <-x$get()
  i_m <-solve(data,...)
  x$setinverse(i_m)  # sets cached value
  i_m
}
