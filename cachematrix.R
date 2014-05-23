## These two functions create a special object that stores a matrix and
## cache's its inverse.

## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
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


## This function computes the inverse of the "matrix" returned by
## the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
               message("getting cached data")
               return(m)
        }
        data <- x$get()
        m<-solve(data)
        x$setinv(m)
        m
}
