## Programming Assignment 2

makeCacheMatrix <- function(x= matrix()) {
    s <- NA
    set <- function(y) {
        x <<- y
        s <<- NA 
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

cacheSolve <- function(x, ...) {
  
    s <- x$getinverse
    if(!is.na(s)) {
        print("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
}
