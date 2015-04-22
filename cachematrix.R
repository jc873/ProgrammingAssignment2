## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following functions are used to cache an inverted matrix.



## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to:
##   - set the matrix
##   - get the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## set initial value of inverseMatrix to NULL -> nothing in cache
    inverseMatrix <- NULL
    
    ## set function
    set <- function(myMatrix) {

        ## write matrix x
        x <<- myMatrix

        ## no cached value -> set to NULL
        inverseMatrix <<- NULL
    }

    ## get function - returns matrix x
    get <- function() x

    ## write inverse matrix to "cache"
    setinverse <- function(inverse) inverseMatrix <<- inverse

    ## read and return inverse matrix from "cache"
    getinverse <- function() inverseMatrix

    ## return the list containing functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a special "matrix" created with
## makeCacheMatrix.
##
## Description:
## - check if inverse matrix has already been calculated (=cached)
##   - if cached: return cached inverted matrix
##   - else     : calculate inverse matrix using solve() an write it to cache
## Returns a matrix that is the inverse of 'x'
##
## Assumption: every matrix can be inverted. (no error handling if not)
cacheSolve <- function(x, ...) {

    ## get data from inverse Matrix (if cached)
    myInverseMatrix <- x$getinverse()

    ## if the inverse matrix ist cached, return cached data
    if(!is.null(myInverseMatrix)) {
        ## print message, return data later
        message("getting cached data")
    }
    else ## getinverse returned NULL -> inversed matrix is not cached
    {
        ## we need to get the matrix
        data <- x$get()
        
        ## calculate inverse matrix
        myInverseMatrix <- solve(data, ...)
        
        ## write inverse matrix to cache
        x$setinverse(myInverseMatrix)
    }

    ## Return the value that is the inverse of 'x'
    myInverseMatrix
}

## ############################################################################
## 
## Testdata / test run
##
## Matrix - pass as argument:
## x = rbind(c(1, -2), c(-2, 1))
##
## > x
## [,1] [,2]
## [1,]    1   -2
## [2,]   -2    1
## > solve(x)
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333
##
## Call makeCacheMatrix 
## m <- makeCacheMatrix (x)
##
## > m$get()
## [,1] [,2]
## [1,]    1   -2
## [2,]   -2    1
##
##
## Call cacheSolve
## cacheSolve(m)
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333
## > cacheSolve(m)
## getting cached data
## [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333
## 