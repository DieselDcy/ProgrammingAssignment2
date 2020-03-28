## The following function is used to set the value of the matrix,
## get the value of the matrix,
##set the value of the inverse,
##get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y){
        
        x<<-y
        m<<-NULL
        
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##The following function checks to see if the inverse of the matrix has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data 
##and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setinverse(m)
    m
}
