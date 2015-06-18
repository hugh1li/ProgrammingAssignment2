## Put comments here that give an overall description of what your
## functions do
#Cache the inverse of a matrix to speed up computation. 

## Write a short comment describing this function
#Create a special 'matrix', which is a list containing a function to 1&2: set the value of the matrix; 3&4: set and get the value of 
#the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinv<-function(solve) m<<-solve
  getinv<-function()
  list(set=set,get=get,
         setinv=setinv,
         getinv=getinv)
}


## Write a short comment describing this function
#The function first checkes ot see if the inverse matrix has already been calculated. if so, it gets the mean from the cache and skip the computation. 
#Otherwise, it calculates the inverse and sets the value in the cache via the setinv function 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
  
}
