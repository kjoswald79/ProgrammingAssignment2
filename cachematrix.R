## The two functions will cache the inverse of a matrix

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x<<-y
        i<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<-inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix created in makeCacheMatrix
## If the inverse has already been calculated, cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i<-x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-inverse(data, ...)
    x$setmean(i)
    i
}
