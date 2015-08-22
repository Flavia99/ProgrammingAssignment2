## This R function is able to cache potentially the inverse of a matrix.

## The first function,makeCacheMatrix creates a "special" matrix,
##which is really a list containing a function to set the value of the matrix
##get the value of the Matrix, set the value of the inverse matrix
##and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)m<<-inverse
  getinverse<-function()m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## The second function, cacheSolve calculates the inverse of the special "matrix" returned
##by the function above. If the inverse has already been calculated, and
##the matrix has not changed,this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
