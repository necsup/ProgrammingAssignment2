## These 2 functions are used to cache and return the inverse of a matrix. The makeCacheMatrix is a list of functions used to store a matrix
## and cache its inverse once calculated (and again if the matrix is multipled). The function cacheSolve returns the invese if it is already cached, 
## otherwise, it calcualtes the invese of the matrix,  caches it for future use, and returns it. 

# This function stores a matrix and its corresponding inverse matrix. 
# set(x=matrix()) sets the values of the input matrix and intializes/resets the mean to NULL when a new matrix is set. 
# get() returns the initial matrix getInverted returns the inverted matrix (returns NULL if # not computed yet).
# setInverse(i = matrix) assignes the invererted matrix to i
# getInverse resturns the inverted matrix
# It is used to provide direct access to the inverse without recomputing it each time

makeCacheMatrix <- function(x = matrix) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function takes as input the function makeCacheMatrix (not that makeCacheMatrix has to be instantiated before with a matrix for x).
# The function then returns the invese of the matrix stored as x in makeCacheMatrix. It returns the cached value directly from makeCacheMatrix
# if the inverse was already# calculated, otherwise, it calcuates the inverse of matrix x, caches it in makeCacheMatrix, and the returns the inverse.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("inverse already exists, getting cached data")
            return(i)
        }
        i <- x$get()
        i <- solve(i, ...)
        x$setinverse(i)
        i
}
