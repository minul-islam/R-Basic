## There are two functions here - makeCacheMatrix and cacheSolve while first one gets and stores the inverser of a matrix. the 
## one inverse a matrix if its a new one

## This is the first function that gets a matrix, stroes it and subsequently get and store the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(InvMat) m<<-InvMat 
  getInv <- function() m
  list(set=set, get =get, setInv=setInv, getInv=getInv)
}


## This is the second function that find the inverser of the matrix receieved through the first function and if the matrix remain 
## unchnaged, gets the old result from the first function otherwise solve the new matrix and stroe the solution in cache
cacheSolve <- function(x, ...) {
        m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve (data, ...)
  x$setInv(m)
  m
}
