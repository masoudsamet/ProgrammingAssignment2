## Overall makeCacheMatrix will create a matrix that can cache its inverse.
## cacheSolve will compute the inverse of matrix returned by makeCacheMatrix. 

## This function create a matrix that can cache its iverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## This function will compute the inverse of a matrix 
## that has been returned by makeCacheMatrix, if its inverse has not be already
## calculated. If the inverse is already in the cache it will retrive that.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m  ## Return a matrix that is the inverse of 'x'
}
