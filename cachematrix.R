## cachematrix.R: program that caches and returns the inverse of a matrix.
## caching the inverse of a matrix is very useful, because its reduce the time spend to operate large matrices.

## makeCacheMatrix: creates a list that containing the following functions:
## 1. Set the values in the matrix (set)
## 2. Get the values of the matrix (get)
## 3. Set the values for the inverse of the matrix (setMatrixInverse)
## 4. Get the values of the inverse of the matrix (getMatrixInverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(matrixInverse) m <<- matrixInverse
        getMatrixInverse <- function() m
        list(set=set, get=get, setMatrixInverse=setMatrixInverse, 
        getMatrixInverse=getMatrixInverse)
}


## cacheSolve: to calculate the inverse the matrix created with makeCacheMatrix function. Firstly, it checks if the inverse is 
## already computed. If so, it gets the inverse from the cache and skips the computation. Otherwise, it computes the inverse and sets the matrix
## in the cache with the setMatrixInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInverse()
        if(!is.null(m)){
                message("getting cached data!")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setMatrixInverse(m)
        m
}
