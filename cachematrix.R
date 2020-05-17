## These functions are compute and cache inversion of a matrix
## 

## This fuction can cache inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInv <- function(mean) Inv <<- solve
    getInv <- function() Inv
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## This function computes the inverse or retrieve it from the cache

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInv(Inv)
    Inv 
}
