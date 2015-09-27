## This program is degined to generally show how to cache some before calculated information for a data set. 
## Specifically this code calculate the inverse of a matrix and will cach it, if it is not calculated before. Otherwise, it retreives the cached value.

## the makeCacheMatrix function is designed to cache the input matrix (x) and the inverse matrix (m). 
## This function creates a list containing a function to 
## 1) set the values of the matrix, 2) get the value of the matrix, 3) set the value of the inverse, and 4) get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) { #set the values of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # get the value of the matrix
        setsolve <- function(solve) m <<- solve # set the value of the inverse
        getsolve <- function() m # get the value of the inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function is calculating the inverse of a matrix, which is the output of the makeCacheMatrix function. 
## This function first checks whether the inverse matrix has been calculated before or not. 
## If it is calculated before, then the function returns the inverse matrix, cached in the list. Otherwise, it calculates the inverse of the matrix and cashes the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #solve function returns the inverse of the matrix. 
        x$setsolve(m)
        m
}