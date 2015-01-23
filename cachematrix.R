## These two functions together able to give the inverse matrix of another
## If once it already was calculated, the value will be stored in makeCacheMatrix function environment

## This function a special type of matrix object with functions to store the inverse

makeCacheMatrix <- function(x = matrix()) {
  stopifnot(is.matrix(x))
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInvers<-function(solve) m<<-solve
  #function to input the invert value (not calculate just store!)
  getInvers<-function() m
  list(set=set,get=get,
       setInvers=setInvers,
       getInvers=getInvers)
}


## This function able to calculate the inverse of the output object "matrix" defined in the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Check if there any inverse stored already
  m<-x$getInvers() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    #function stops, m value returns
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInvers(m)
  #value of m stored in the environment of setinvers function
  m
}
