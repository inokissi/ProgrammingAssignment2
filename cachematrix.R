## In this Programming Assignment took advantage of the scoping rules of the 
## R language and how they can be manipulated to preserve state inside of an R object.
## The following functions cache the inverse of a matrix.
## For this assignment, we assumed that the matrix supplied is always invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL             
  set <- function(y){
    x <<- y             
    m <<- NULL
  }
  get<-function() x
  setinvmatrix<-function(solve) m <<- solve
  getinvmatrix<-function() m
  list(set=set, get=get,
       setinvmatrix=setinvmatrix,
       getinvmatrix=getinvmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinvmatrix(m)
  m
}
