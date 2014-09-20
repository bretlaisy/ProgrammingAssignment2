## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #set the value of the matrix
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  #get the value of the matrix
  get<-function() x
  
  #set the inverse of the matrix
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  #get the inverse of the matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## omputes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x=matrix(), ...) {
  
  #get the inverse of the matrix
  m<-x$getmatrix()
  
  #Check if there is any matrix
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  #Else get the inverse of the matrix
  matrix<-x$get()
  m<-solve(matrix, ...)
  
  #set the inverse of the matrix
  x$setmatrix(m)
  m
}
