## These two functions create a special object that stores a matrix and
## cache's its inverse.

## The function makeCacheMatrix creates a "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
              ## initialization
              m<-NULL
              set <- function(y) {
                x<<-y
                m<<-NULL
              }
              get <-function() x
              setinv<-function(invf) m<<-invf
              getinv<-function() m
              list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The function cacheSolve computes the inverse of the "matrix" 
## returned by the function makeCacheMatrix

cacheSolve <- function(x, ...) {

        m <- x$getinv()
        ## checks if returned cache has anything in it
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }
        data <- x$get()
        ##  computes the inverse of 'x'
        m<-solve(data)
        x$setinv(m)
        m
}
