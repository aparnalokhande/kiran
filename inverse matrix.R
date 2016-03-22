## following function creats matrix  and calculate its inverse

rm(list=ls())
makeCacheMatrix <- function(M = matrix()) {
    inv <- NULL
    set <- function(x) {
        M<<- x
        inv <<- NULL
    }
    get <- function() M
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Folowing function Return a matrix that is the inverse of 'x'
cacheSolve <- function(M, ...) {
    inv <- M$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- M$get()
    inv <- solve(data)
    M$setinverse(inv)
    inv
}
## Example 
my_matrix <- makeCacheMatrix(matrix(10:7, 2, 2))

my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
