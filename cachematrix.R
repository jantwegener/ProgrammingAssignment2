## The purpose of this R script is to have a matrix that is able to cache its inverse.
## 
## The inverse of a square matrix A is a square matrix B so that AB = I, where I is the identity matrix.
## Note that not every square matrix has an inverse (e.g., matrix(rep(0, 2*2), 2, 2) does not have an inverse matrix).
## This script assumes that the given matrix A is a square matrix and that A has an inverse matrix.
## 
## 
## 
## In order to test the script, it is usually sufficient to create a square matrix whose entries are filled by random variables. This works due to the following statement taken from wikipedia.org:
## "A square matrix that is not invertible is called singular or degenerate. A square matrix is singular if and only if its determinant is 0. Singular matrices are rare in the sense that a square matrix randomly selected from a continuous uniform distribution on its entries will almost never be singular." [https://en.wikipedia.org/wiki/Invertible_matrix]





## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ## m = a matrix
        
        # For this assignment, assume that the matrix supplied is always invertible.
        
        # the cached inverse matrix
        minv <<- NULL
        setMatrix <- function(m_) {
                m <<- m_
                minv <<- NULL
        }
        getMatrix <- function() {
                m
        }
        setInv <- function(minv_) {
                minv <<- minv_
        }
        getInv <- function() {
                minv
        }
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## m = a matrix created with makeCacheMatrix
        minv <- m$getInv()
        if (is.null(minv)) {
                print("compute")
                # not cached so compute
                minv <- solve(m$getMatrix(), ...)
                # now we have to cache the matrix
                m$setInv(minv)
        }
        minv
}
