## The purpose of this R script is to have a matrix that is able to cache its inverse.
## 
## The inverse of a square matrix A is a square matrix B so that AB = I, where I is the identity matrix.
## Note that not every square matrix has an inverse (e.g., matrix(rep(0, 2*2), 2, 2) does not have an inverse matrix).
## This script assumes that the given matrix A is a square matrix and that A has an inverse matrix.
## 
## The script contains two functions, makeCacheMatrix and cacheSolve.
## The function makeCacheMatrix creates a new object for caching a matrix and its inverse matrix.
## The function cacheSolve returns the inverse matrix of a matrix created with makeCacheMatrix. It either returns the cached value or it computes the inverse matrix if the cache is empty or not up to date.
## The code for the functions is highly inspired by the given example in https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md.
## 
## In order to test the functions of this script, it is usually sufficient to create a square matrix whose entries are filled by random variables. This works due to the following statement taken from wikipedia.org:
## "A square matrix that is not invertible is called singular or degenerate. A square matrix is singular if and only if its determinant is 0. Singular matrices are rare in the sense that a square matrix randomly selected from a continuous uniform distribution on its entries will almost never be singular." [https://en.wikipedia.org/wiki/Invertible_matrix]
##
## The code can be tested as follows:
##
### the k must be big enough to "see" the caching effect (may need to be adjusted for your PC)
## k <- 1200
### initialize a matrix
## mraw <- matrix(rep(1, k*k), k, k)
### add/sub white noise, most of the times this matrix is invertible
## m <- matrix(sapply(mraw, function(x) x + runif(1, -2.0, 2.0)), k, k)
## 
## cm <- makeCacheMatrix(m)
### compute the inverse
## x <- cacheSolve(cm)
### return the cached inverse, the runtime difference should be noticable
## x <- cacheSolve(cm)
## 
### create a new matrix m1 as described above and set the new matrix to cm
## mraw <- matrix(rep(1, k*k), k, k)
## m1 <- matrix(sapply(mraw, function(x) x + runif(1, -2.0, 2.0)), k, k)
## cm$setMatrix(m1)
### compute the inverse since the matrix changed
## x <- cacheSolve(cm)
### return the cached inverse, the runtime difference should be noticable
## x <- cacheSolve(cm)


## This function returns a list which provides the access functions to a cache matrix object.
## The input must be an invertible, square matrix.
## To access the cached inverse matrix, the method cacheSolve() should be called.
##
## The functions in the returned list are: setMatrix, getMatrix, setInv, and getInv.
## setMatrix: sets a new matrix to be cached
## getMatrix: returns the currently cached matrix
## setInv: set the inverse matrix (should not be called directly)
## getInv: returns the currently stored inverse matrix (should not be called directly)
makeCacheMatrix <- function(x = matrix()) {
        ## x = a matrix
        # the cached inverse matrix
        minv <<- NULL
        # set the matrix
        setMatrix <- function(m_) {
                x <<- m_
                minv <<- NULL
        }
        # returns the matrix
        getMatrix <- function() {
                x
        }
        # sets the inverse matrix
        setInv <- function(minv_) {
                minv <<- minv_
        }
        # returns the inverse matrix
        getInv <- function() {
                minv
        }
        # the list of methods
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}


## This method returns the inverse matrix of the given matrix.
## When the inverse matrix has already been computed the cached value is returned, otherwise the inverse matrix is computed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## x = a matrix created with makeCacheMatrix
        minv <- x$getInv()
        if (is.null(minv)) {
                # not cached so compute
                minv <- solve(x$getMatrix(), ...)
                # now we have to cache the matrix
                x$setInv(minv)
        }
        # return the inverse matrix
        minv
}
