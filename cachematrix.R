## This function creates a special "matrix" object that can cache its inverse.
## based on example cacheMeanVector
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse #
    getinv <- function() inv#
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## test set from https://class.coursera.org/rprog-003/forum/thread?thread_id=511
## 
# source("cachematrix.R")     # source the functions
# a <- makeCacheMatrix()      # initialize
# a                           # shows that a is now a list of functions
# class(a)                    # shows that a is a list
# class(a$set)                # shows that the elements of the list are functions
# a$set(matrix(1:4,2,2))      # set the matrix
# a$get                       # get the matrix 
# cacheSolve(a)               # calculate the inverse 
# cacheSolve(a)               # when called back, use the cached inverse 
