# This is only adaptation of functions makeVector and cacheMean to
# requirements of Assignment 2. 

# makeCacheMatrix takes square invertible matrix as input and creates
# "special" matrix - list of functions that set and get value of 
# our matrix and set and get the value of an invers matrix

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve takes as an argument special matrix created by 
# makeCacheMatrix, chcecks if inverse matrix was computed soon
# and unless it was, computes and return it

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
