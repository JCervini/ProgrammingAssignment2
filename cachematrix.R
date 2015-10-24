## Author: J. Cervini
## Course: Coursera R Programming
## These are two functions that work in tandem to allow one to store a cached inverse
##     of a matrix (so we don’t have to keep calculating it).
## makeCacheMatrix() creates a list for storing the matrix and caching its inverse
## cacheSolve() takes above function’s output and will return a cached inverse 
##            or calculate the inverse, cache it, then return it
## example: m  <-  matrix(1:4)
##          cm <-  makeCacheMatrix(m)
##          inv <- cacheSolve(cm)
##
##
## makeCacheMatrix: this function  creates the list to store the matrix, 
##     its inverse, and the global functions used in cacheSolve.
## Input:   matrix (assumed square, and has inverse)
## Returns: list
## 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## create 4 functions
          ## set        initial setting of matrix to x, inverse to null
          ## get        retrieves the stored matrix from list
          ## setinverse stores the inverse in list
          ## getinverse retrieves the inverse from list
        set        <- function(y) {
                       x <<- y
                       m <<- NULL }
        get        <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m

        ## function returns a list with 4 functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: this function uses the global list to return cached inverse
##     or to calculate, store, and return it
## Input:   list variable (from makeCacheMatrix)
## Returns: inverse of matrix stored (or inverse calculated & stored & returned)
cacheSolve <- function(x, ...) {
        ## from global variable get the stored inverse
        m <- x$getinverse()

        ## if stored value is not empty return inverse back
        if(!is.null(m)) {
           message("getting cached data")
           return(m)
        }
        ## else retrieve stored matrix
        ## calculate inverse, store inverse in global variable, return inverse
        else {
           data <- x$get()
           m <- solve(data, ...)
           x$setinverse(m)
           return(m)
        }
}


