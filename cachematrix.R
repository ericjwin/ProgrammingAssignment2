## Coursera R Programming
## Programming Assignment 2: Caching the Inverse of a Matrix
## This is a very simple algorithm with no error checking.
##             Note: only works with SQUARE matrices
##
## Eric W. Johnson, 1/25/2015
## File cachematrix.R

## Usage:
     ## 1. Create a square matrix e.g. x<-matrix(sample(1:100,9),ncol=3)
     ## 2. Turn that into a "CacheMatrix" e.g. xcm<-makeCacheMatrix(x)
     ## 3. Call cacheSolve(xcm) to get the inverse matrix of xcm
               ## Note: given these steps, cacheSolve(xcm) equals solve(x)

## Common Error Messages:
     ## Calling cacheSolve on a matrix that is not a "CacheMatrix" gives an error
     ##        "Error in x$getinverse : $ operator is invalid for atomic vectors"
     ##
     ## a<-matrix(1:9,ncol=3) #creates a 3x3 matrix with sequential integers
     ## solve(a) #gives an error message because the determinant of a, det(a), is zero.
     ##                  Lapack routine dgesv: system is exactly singular
     ##
     ## a<-matrix(1:8,ncol=2) #creates a 4x2 (non-square) matrix with sequential integers
     ## det(a) #gives an error message Error in determinant.matrix(x, logarithm = TRUE, ...) : 
     ##                  'x' must be a square matrix

################################################################################
## Thisfunction creates a special "CacheMatrix" object that can cache its inverse.
## A "CacheMatrix" object, xcm, has five "methods"
     ## xcm$get() returns the xcm matrix
     ## xcm$set(x) sets (initializes) the value of xcm to the matrix x
     ## xcm$getinverse() gets (returns) the inverse of xcm
     ## xcm$setinverse(x) sets (initializes) the inverse of xcm to the matrix x
     ## xcm$ischanged() returns TRUE if matrix x is new or changed
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL        ## local variable to store the inverse of x
     isch <- TRUE     ## local variable to store the ischanged value
     set <- function(y) {
          x <<- y
          m <<- NULL
          isch <<-TRUE
     }
     get <- function(){ x }
     setinverse <- function(inverse){ isch <<-FALSE; m <<- inverse }
     getinverse <- function(){ m }
     ischanged <- function(){ isch }
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse,
          ischanged = ischanged)
}


################################################################################
## This function computes the inverse of the special "CacheMatrix" object
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.
## cacheSolve calls the methods of the "CacheMatrix" object passed in.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!x$ischanged()) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinverse(m)
     m
}
